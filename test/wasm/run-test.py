#!/usr/bin/env python3
#
# Copyright 2017 WebAssembly Community Group participants
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

# Modified version of wabt's run-spec-wasm2c.py, adapted for cmm_of_wasm.
# Changes:
#  1. Support Python 3 only
#  2. Directory structure changes (remove find_exe dependency)
#  3.
import argparse
from io import StringIO
import json
import os
import re
import struct
import subprocess
import sys
import hashlib

import utils
from utils import Error

TEST_DIR = os.path.dirname(os.path.abspath(__file__))
ROOT_DIR = os.path.abspath(os.path.join(TEST_DIR, '..', '..'))
WASMRT_DIR = os.path.join(ROOT_DIR, 'src', 'rts')
INCLUDES_DIR = os.path.join(ROOT_DIR, 'includes')
EXTERNAL_BINDIR = os.path.join(ROOT_DIR, 'external', 'bin')
CMM_OF_WASM_EXEC = 'cmm_of_wasm'
WAST_JSON_EXEC = 'wast2json'

def ReinterpretF32(f32_bits):
  return struct.unpack('<f', struct.pack('<I', f32_bits))[0]

def F32ToC(f32_bits):
  F32_SIGN_BIT = 0x80000000
  F32_INF = 0x7f800000
  F32_SIG_MASK = 0x7fffff

  if (f32_bits & F32_INF) == F32_INF:
    sign = '-' if (f32_bits & F32_SIGN_BIT) == F32_SIGN_BIT else ''
    # NaN or infinity
    if f32_bits & F32_SIG_MASK:
      # NaN
      return '%smake_nan_f32(0x%06x)' % (sign, f32_bits & F32_SIG_MASK)
    else:
      return '%sINFINITY' % sign
  elif f32_bits == F32_SIGN_BIT:
    return '-0.f'
  else:
    s = '%.9g' % ReinterpretF32(f32_bits)
    if '.' not in s:
      s += '.'
    return s + 'f'


def ReinterpretF64(f64_bits):
  return struct.unpack('<d', struct.pack('<Q', f64_bits))[0]

def F64ToC(f64_bits):
  F64_SIGN_BIT = 0x8000000000000000
  F64_INF = 0x7ff0000000000000
  F64_SIG_MASK = 0xfffffffffffff

  if (f64_bits & F64_INF) == F64_INF:
    sign = '-' if (f64_bits & F64_SIGN_BIT) == F64_SIGN_BIT else ''
    # NaN or infinity
    if f64_bits & F64_SIG_MASK:
      # NaN
      return '%smake_nan_f64(0x%06x)' % (sign, f64_bits & F64_SIG_MASK)
    else:
      return '%sINFINITY' % sign
  elif f64_bits == F64_SIGN_BIT:
    return '-0.0'
  else:
    return '%.17g' % ReinterpretF64(f64_bits)


def MangleType(t):
  return {'i32': 'i', 'i64': 'j', 'f32': 'f', 'f64': 'd'}[t]


def MangleTypes(types):
  if not types:
    return 'v'
  return ''.join(MangleType(t) for t in types)


def MangleName(s):
  result = 'Z_'
  for c in s:
    # NOTE(binji): Z is not allowed.
    if c in '_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXY0123456789':
      result += c
    else:
      result += 'Z%02X' % ord(c)
  return result

def SanitiseName(name):
  orig_name = name
  # Same as in OCaml.
  if name == "":
    return "_empty_"
  re_valid_start = re.compile("[_a-zA-Z]")

  # Valid start
  if not re_valid_start.match(name[0]):
    name = "_" + name

  # Dots and dashes
  p = re.compile("\.")
  name = p.sub("_dot_", name)
  p = re.compile("-")
  name = p.sub("_dash_", name)

  # Silly characters
  silly_char_re = re.compile("[^_a-zA-Z0-9]")
  if silly_char_re.search(name):
    # Generate md5 digest
    m = hashlib.md5()
    m.update(orig_name.encode("latin1"))
    name_hash = m.hexdigest()
    # Strip out silly chars
    name = silly_char_re.sub("", name)
    # Append digest
    #print("silly string", name, "hash", name_hash)
    name = name + "_" + name_hash
  return name

class CWriter(object):

  def __init__(self, spec_json, prefix, out_file, out_dir, mangle):
    self.source_filename = os.path.basename(spec_json['source_filename'])
    self.commands = spec_json['commands']
    self.out_file = out_file
    self.out_dir = out_dir
    self.prefix = prefix
    self.module_idx = 0
    self.module_name_to_idx = {}
    self.module_prefix_map = {}
    self.mangle = mangle

  def Write(self):
    self._MaybeWriteDummyModule()
    self._CacheModulePrefixes()
    self._WriteIncludes()
    self.out_file.write(self.prefix)
    self.out_file.write("\nvoid run_spec_tests(void) {\n\n")
    for command in self.commands:
      self._WriteCommand(command)
    self.out_file.write("\n}\n")

  def GetModuleFilenames(self):
    return [c['filename'] for c in self.commands if c['type'] == 'module']

  def GetModulePrefix(self, idx_or_name=None):
    if idx_or_name is not None:
      return self.module_prefix_map[idx_or_name]
    return self.module_prefix_map[self.module_idx - 1]

  def _CacheModulePrefixes(self):
    idx = 0
    for command in self.commands:
      if command['type'] == 'module':
        name = os.path.basename(command['filename'])
        name = os.path.splitext(name)[0]
        name = SanitiseName(name)
        if self.mangle:
          name = MangleName(name)

        self.module_prefix_map[idx] = name

        if 'name' in command:
          self.module_name_to_idx[command['name']] = idx
          self.module_prefix_map[command['name']] = name

        idx += 1
      elif command['type'] == 'register':
        if self.mangle:
          name = MangleName(command['as'])
        else:
          name = command['as']

        if 'name' in command:
          self.module_prefix_map[command['name']] = name
          name_idx = self.module_name_to_idx[command['name']]
        else:
          name_idx = idx - 1

        self.module_prefix_map[name_idx] = name

  def _MaybeWriteDummyModule(self):
    if len(self.GetModuleFilenames()) == 0:
      # This test doesn't have any valid modules, so just use a dummy instead.
      filename = utils.ChangeExt(self.source_filename, '-dummy.wasm')
      with open(os.path.join(self.out_dir, filename), 'wb') as wasm_file:
        wasm_file.write(b'\x00\x61\x73\x6d\x01\x00\x00\x00')

      dummy_command = {'type': 'module', 'line': 0, 'filename': filename}
      self.commands.insert(0, dummy_command)

  def _WriteFileAndLine(self, command):
    self.out_file.write('// %s:%d\n' % (self.source_filename, command['line']))

  def _WriteIncludes(self):
    idx = 0
    for filename in self.GetModuleFilenames():
      header = os.path.splitext(filename)[0] + '.h'
      self.out_file.write(
          '#define WASM_RT_MODULE_PREFIX %s\n' % self.GetModulePrefix(idx))
      self.out_file.write("#include \"%s\"\n" % header)
      self.out_file.write('#undef WASM_RT_MODULE_PREFIX\n\n')
      idx += 1

  def _WriteCommand(self, command):
    command_funcs = {
        'module': self._WriteModuleCommand,
        'action': self._WriteActionCommand,
        'assert_return': self._WriteAssertReturnCommand,
        'assert_return_canonical_nan': self._WriteAssertReturnNanCommand,
        'assert_return_arithmetic_nan': self._WriteAssertReturnNanCommand,
        'assert_trap': self._WriteAssertActionCommand,
        'assert_exhaustion': self._WriteAssertActionCommand,
    }

    func = command_funcs.get(command['type'])
    if func is not None:
      self._WriteFileAndLine(command)
      func(command)
      self.out_file.write('\n')

  def _WriteModuleCommand(self, command):
    self.module_idx += 1
    self.out_file.write('%s_init();\n' % self.GetModulePrefix())

  def _WriteActionCommand(self, command):
    self.out_file.write('%s;\n' % self._Action(command))

  def _WriteAssertReturnCommand(self, command):
    expected = command['expected']
    if len(expected) == 1:
      assert_map = {
        'i32': 'ASSERT_RETURN_I32',
        'f32': 'ASSERT_RETURN_F32',
        'i64': 'ASSERT_RETURN_I64',
        'f64': 'ASSERT_RETURN_F64',
      }

      type_ = expected[0]['type']
      assert_macro = assert_map[type_]
      self.out_file.write('%s(%s, %s);\n' %
                          (assert_macro,
                           self._Action(command),
                           self._ConstantList(expected)))
    elif len(expected) == 0:
      self._WriteAssertActionCommand(command)
    else:
      raise Error('Unexpected result with multiple values: %s' % expected)

  def _WriteAssertReturnNanCommand(self, command):
    assert_map = {
      ('assert_return_canonical_nan', 'f32'): 'ASSERT_RETURN_CANONICAL_NAN_F32',
      ('assert_return_canonical_nan', 'f64'): 'ASSERT_RETURN_CANONICAL_NAN_F64',
      ('assert_return_arithmetic_nan', 'f32'): 'ASSERT_RETURN_ARITHMETIC_NAN_F32',
      ('assert_return_arithmetic_nan', 'f64'): 'ASSERT_RETURN_ARITHMETIC_NAN_F64',
    }

    expected = command['expected']
    type_ = expected[0]['type']
    assert_macro = assert_map[(command['type'], type_)]

    self.out_file.write('%s(%s);\n' % (assert_macro, self._Action(command)))

  def _WriteAssertActionCommand(self, command):
    assert_map = {
      'assert_exhaustion': 'ASSERT_EXHAUSTION',
      'assert_return': 'ASSERT_RETURN',
      'assert_trap': 'ASSERT_TRAP',
    }

    assert_macro = assert_map[command['type']]
    self.out_file.write('%s(%s);\n' % (assert_macro, self._Action(command)))

  def _Constant(self, const):
    type_ = const['type']
    value = int(const['value'])
    if type_ == 'i32':
      return '%su' % value
    elif type_ == 'i64':
      return '%sull' % value
    elif type_ == 'f32':
      return F32ToC(value)
    elif type_ == 'f64':
      return F64ToC(value)
    else:
      assert False

  def _ConstantList(self, consts):
    return ', '.join(self._Constant(const) for const in consts)

  def _ActionSig(self, action, expected):
    type_ = action['type']
    result_types = [result['type'] for result in expected]
    arg_types = [arg['type'] for arg in action.get('args', [])]
    if type_ == 'invoke':
      return MangleTypes(result_types) + MangleTypes(arg_types)
    elif type_ == 'get':
      return MangleType(result_types[0])
    else:
      raise Error('Unexpected action type: %s' % type_)

  def _Action(self, command):
    action = command['action']
    expected = command['expected']
    type_ = action['type']
    module_name = self.GetModulePrefix(action.get('module'))
    if self.mangle:
      field = (module_name + MangleName(action['field']) +
               MangleName(self._ActionSig(action, expected)))
    else:
      field = SanitiseName(action['field'])
    if type_ == 'invoke':
      field = module_name + "_cfunc_" + field
      return '%s(%s)' % (field, self._ConstantList(action.get('args', [])))
    elif type_ == 'get':
      field = module_name + "_cglobal_" + field
      return '*%s' % field
    else:
      raise Error('Unexpected action type: %s' % type_)


def Compile(cc, c_filename, out_dir, *args):
  out_dir = os.path.abspath(out_dir)
  o_filename = utils.ChangeDir(utils.ChangeExt(c_filename, '.o'), out_dir)
  cc.RunWithArgs('-c', '-o', o_filename, c_filename, *args, cwd=out_dir)
  return o_filename


def Link(cc, o_filenames, main_exe, out_dir, *args):
  args = ['-o', main_exe] + o_filenames + list(args)
  cc.RunWithArgs(*args, cwd=out_dir)

def wast2json_executable(bin_dir):
  return os.path.join(bin_dir, WAST_JSON_EXEC)

def cmm_of_wasm_executable(cmm_dir):
  return os.path.join(cmm_dir, CMM_OF_WASM_EXEC)

def main(args):
  parser = argparse.ArgumentParser()
  parser.add_argument('-o', '--out-dir', metavar='PATH',
                      help='output directory for files.')
  parser.add_argument('-P', '--prefix', metavar='PATH', help='prefix file.',
                      default=os.path.join(INCLUDES_DIR, 'spec-wasm2c-prefix.c'))
  parser.add_argument('--external-bin-dir', metavar='PATH',
                      default=EXTERNAL_BINDIR,
                      dest='external_bin_dir',
                      help='directory to search for all executables.')
  parser.add_argument('--compiler-bin-dir',
                      help='directory containing cmm_of_wasm',
                      default=ROOT_DIR,
                      dest='compiler_dir')
  parser.add_argument('--wasmrt-dir', metavar='PATH',
                      help='directory with wasm-rt files', default=WASMRT_DIR,
                      dest='wasmrt_dir')
  parser.add_argument('--cc', metavar='PATH',
                      help='the path to the C compiler', default='cc')
  parser.add_argument('--cflags', metavar='FLAGS',
                      help='additional flags for C compiler.',
                      action='append', default=[])
  parser.add_argument('--compile', help='compile the C code (default)',
                      dest='compile', action='store_true')
  parser.add_argument('--no-compile', help='don\'t compile the C code',
                      dest='compile', action='store_false')
  parser.set_defaults(compile=True)
  parser.add_argument('--no-run', help='don\'t run the compiled executable',
                      dest='run', action='store_false')
  parser.add_argument('-v', '--verbose',
                      help='print more diagnostic messages.',
                      action='store_true')
  parser.add_argument('--no-error-cmdline',
                      help='don\'t display the subprocess\'s commandline when'
                      + ' an error occurs', dest='error_cmdline',
                      action='store_false')
  parser.add_argument('-p', '--print-cmd',
                      help='print the commands that are run.',
                      action='store_true')
  parser.add_argument('-m', '--mangle',
                      help='mangle names',
                      action='store_true')
  parser.add_argument('-t', '--keep-temp',
                      help='keep temporary files',
                      action='store_true')
  parser.add_argument('file', help='wast file.')
  options = parser.parse_args(args)

  with utils.TempDirectory(options.out_dir, 'cmm_of_wasm-test-') as out_dir:
    # Parse JSON file and generate main .c file with calls to test functions.
    #print("out_dir: ", out_dir)
    wast2json = utils.Executable(
        wast2json_executable(options.external_bin_dir),
        error_cmdline=options.error_cmdline)
    wast2json.AppendOptionalArgs({'-v': options.verbose})

    json_file_path = utils.ChangeDir(
        utils.ChangeExt(options.file, '.json'), out_dir)
    wast2json.RunWithArgs(options.file, '-o', json_file_path)

    cmm_of_wasm = utils.Executable(
       cmm_of_wasm_executable(options.compiler_dir),
       error_cmdline=options.error_cmdline)

    cc = utils.Executable(options.cc, *options.cflags)

    with open(json_file_path) as json_file:
      spec_json = json.load(json_file)

    prefix = ''
    if options.prefix:
      with open(options.prefix) as prefix_file:
        prefix = prefix_file.read() + '\n'

    output = StringIO()
    cwriter = CWriter(spec_json, prefix, output, out_dir, options.mangle)
    cwriter.Write()

    main_filename = utils.ChangeExt(json_file_path,  '-main.c')
    with open(main_filename, 'w') as out_main_file:
      out_main_file.write(output.getvalue())

    o_filenames = []
    includes = '-I%s' % options.wasmrt_dir

    # Compile wasm-rt-impl.
    wasm_rt_impl_c = os.path.join(options.wasmrt_dir, 'wasm-rt-impl.c')
    o_filenames.append(Compile(cc, wasm_rt_impl_c, out_dir, includes))

    for i, wasm_filename in enumerate(cwriter.GetModuleFilenames()):
      chopped_filename = os.path.splitext(os.path.basename(wasm_filename))[0]
      prefix = cwriter.GetModulePrefix(i)
      if options.keep_temp:
          cmm_of_wasm.RunWithArgs('-o', chopped_filename, '-p', prefix, wasm_filename, '-tv', cwd=out_dir)
      else:
          cmm_of_wasm.RunWithArgs('-o', chopped_filename, '-p', prefix, wasm_filename, cwd=out_dir)

      o_filenames.append(chopped_filename + '.o')

    if options.compile:
      main_c = os.path.basename(main_filename)
      o_filenames.append(Compile(cc, main_c, out_dir, includes))
      main_exe = os.path.basename(utils.ChangeExt(json_file_path, ''))
      Link(cc, o_filenames, main_exe, out_dir, '-lm')

    if options.compile and options.run:
      print("Running", os.path.join(out_dir, main_exe))
      utils.Executable(os.path.join(out_dir, main_exe),
                       forward_stdout=True).RunWithArgs()

  return 0


if __name__ == '__main__':
  try:
    sys.exit(main(sys.argv[1:]))
  except Error as e:
    sys.stderr.write(str(e))
    sys.exit(1)
