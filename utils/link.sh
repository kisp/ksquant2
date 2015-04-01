# This file is part of KSQuant2.

# Copyright (c) 2010 - 2011, Kilian Sprotte. All rights reserved.

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

/usr/bin/gcc-4.0 -v -o main -DDONT_WANT_WIN32_DLL_SUPPORT /opt/local/lib/libgmp.a Main.o ./Interval.o ./MeasureToLily.o ./Lisp.o ./AbstractScore.o ./Lily.o ./Measure.o ./Utils.o ./MeasureToEnp.o ./Enp.o ./SimpleFormat2.o ./SimpleFormat.o ./Input.o -L/opt/local/lib -L/opt/local/lib/ghc-6.10.4/parsec-2.1.0.1 -L/opt/local/lib -L/opt/local/lib/ghc-6.10.4/base-4.1.0.0 -L/opt/local/lib/ghc-6.10.4/integer-0.1.0.1 -L/opt/local/lib/ghc-6.10.4/ghc-prim-0.1.0.0 -L/opt/local/lib/ghc-6.10.4 -lHSparsec-2.1.0.1 -lHSbase-4.1.0.0 -lHSinteger-0.1.0.1 -lHSghc-prim-0.1.0.0 -lHSrts -lm -lHsLocalFfi -ldl -u _ghczmprim_GHCziTypes_Izh_static_info -u _ghczmprim_GHCziTypes_Czh_static_info -u _ghczmprim_GHCziTypes_Fzh_static_info -u _ghczmprim_GHCziTypes_Dzh_static_info -u _base_GHCziPtr_Ptr_static_info -u _base_GHCziWord_Wzh_static_info -u _base_GHCziInt_I8zh_static_info -u _base_GHCziInt_I16zh_static_info -u _base_GHCziInt_I32zh_static_info -u _base_GHCziInt_I64zh_static_info -u _base_GHCziWord_W8zh_static_info -u _base_GHCziWord_W16zh_static_info -u _base_GHCziWord_W32zh_static_info -u _base_GHCziWord_W64zh_static_info -u _base_GHCziStable_StablePtr_static_info -u _ghczmprim_GHCziTypes_Izh_con_info -u _ghczmprim_GHCziTypes_Czh_con_info -u _ghczmprim_GHCziTypes_Fzh_con_info -u _ghczmprim_GHCziTypes_Dzh_con_info -u _base_GHCziPtr_Ptr_con_info -u _base_GHCziPtr_FunPtr_con_info -u _base_GHCziStable_StablePtr_con_info -u _ghczmprim_GHCziBool_False_closure -u _ghczmprim_GHCziBool_True_closure -u _base_GHCziPack_unpackCString_closure -u _base_GHCziIOBase_stackOverflow_closure -u _base_GHCziIOBase_heapOverflow_closure -u _base_ControlziExceptionziBase_nonTermination_closure -u _base_GHCziIOBase_blockedOnDeadMVar_closure -u _base_GHCziIOBase_blockedIndefinitely_closure -u _base_ControlziExceptionziBase_nestedAtomically_closure -u _base_GHCziWeak_runFinalizzerBatch_closure -u _base_GHCziTopHandler_runIO_closure -u _base_GHCziTopHandler_runNonIO_closure -u _base_GHCziConc_runHandlers_closure -u _base_GHCziConc_ensureIOManagerIsRunning_closure -Wl,-search_paths_first -read_only_relocs warning

otool  -L main
