;;; company-asm.el --- Assembly Company Backends     -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Wu Junyu

;; Author: Wu Junyu <vistar_w@hotmail.com>
;; Keywords: languages, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'company-template)
(require 'cl)
(require 'cl-lib)
(require 'company)

(defconst  company-asm--static-completions
  '(;; Macro Directive
	#("section" 0 1
	  (:annotation
	   "asection"
	   :summary
	   (concat "asm section")))
	#("global" 0 1
	  (:annotation
	   "aglobal"
	   :summary
	   (concat "asm global")))
	#(".text" 0 1
	  (:annotation
	   "adottext"
	   :summary
	   (concat "asm .text")))
	#(".code" 0 1
	  (:annotation
	   "adotcode"
	   :summary
	   (concat "asm .code")))
	#(".data" 0 1
	  (:annotation
	   "adotdata"
	   :summary
	   (concat "asm .data")))
	#(".data?" 0 1
	  (:annotation
	   "adotdata?"
	   :summary
	   (concat "asm .data?")))
	#(".const" 0 1
	  (:annotation
	   "adotconst"
	   :summary
	   (concat "asm .const")))
	#(".fardata" 0 1
	  (:annotation
	   "adotfardata"
	   :summary
	   (concat "asm .fardata")))
	#(".fardata?" 0 1
	  (:annotation
	   "adotfardata?"
	   :summary
	   (concat "asm .fardata?")))
	#(".stack" 0 1
	  (:annotation
	   "adotstack"
	   :summary
	   (concat "asm .stack")))
	#("@model" 0 1
	  (:annotation
	   "a@model"
	   :summary
	   (concat "asm @model")))
	#("@code" 0 1
	  (:annotation
	   "a@code"
	   :summary
	   (concat "asm @code")))
	#("@data" 0 1
	  (:annotation
	   "a@data"
	   :summary
	   (concat "asm @data")))
	#("@fardata" 0 1
	  (:annotation
	   "a@fardata"
	   :summary
	   (concat "asm @fardata")))
	#("@stack" 0 1
	  (:annotation
	   "a@stack"
	   :summary
	   (concat "asm @stack")))
	#("@codesize" 0 1
	  (:annotation
	   "a@codesize"
	   :summary
	   (concat "asm @codesize")))
	#("@datasize" 0 1
	  (:annotation
	   "a@datasize"
	   :summary
	   (concat "asm @datasize")))
	#("org" 0 1
	  (:annotation
	   "aorg"
	   :summary
	   (concat "asm org")))
	#("even" 0 1
	  (:annotation
	   "aeven"
	   :summary
	   (concat "asm even")))
	#("proc" 0 1
	  (:annotation
	   "aproc"
	   :summary
	   (concat "asm proc")))
	#("near" 0 1
	  (:annotation
	   "anear"
	   :summary
	   (concat "asm near")))
	#("far" 0 1
	  (:annotation
	   "afar"
	   :summary
	   (concat "asm far")))
	#("include" 0 1
	  (:annotation
	   "ainclude"
	   :summary
	   (concat "asm include")))
	#("title" 0 1
	  (:annotation
	   "atitle"
	   :summary
	   (concat "asm title")))
	#("macro" 0 1
	  (:annotation
	   "amacro"
	   :summary
	   (concat "asm macro")))
	#("local" 0 1
	  (:annotation
	   "alocal"
	   :summary
	   (concat "asm local")))
	#("extra" 0 1
	  (:annotation
	   "aextra"
	   :summary
	   (concat "asm extra")))
	#("stack" 0 1
	  (:annotation
	   "astack"
	   :summary
	   (concat "asm stack")))
	#("being" 0 1
	  (:annotation
	   "abeing"
	   :summary
	   (concat "asm being")))
	#("code" 0 1
	  (:annotation
	   "acode"
	   :summary
	   (concat "asm code")))
	#("param" 0 1
	  (:annotation
	   "aparam"
	   :summary
	   (concat "asm param")))
	#("_start" 0 1
	  (:annotation
	   "astart"
	   :summary
	   (concat "asm _start")))
	#("segment" 0 1
	  (:annotation
	   "asegment"
	   :summary
	   (concat "asm segemnt")))
	#("end" 0 1
	  (:annotation
	   "aend"
	   :summary
	   (concat "asm end")))
	#("ends" 0 1
	  (:annotation
	   "aends"
	   :summary
	   (concat "asm ends")))
	#("exit" 0 1
	  (:annotation
	   "aexit"
	   :summary
	   (concat "asm exit")))
	#("int" 0 1
	  (:annotation
	   "aint"
	   :summary
	   (concat "asm int")))
	#("assume" 0 1
	  (:annotation
	   "aassume"
	   :summary
	   (concat "asm assume")))
	#("nothing" 0 1
	  (:annotation
	   "anothing"
	   :summary
	   (concat "asm nothing")))
	#("group" 0 1
	  (:annotation
	   "agroup"
	   :summary
	   (concat "asm group")))
	#("model" 0 1
	  (:annotation
	   "amodel"
	   :summary
	   (concat "asm model")))
	#("tiny" 0 1
	  (:annotation
	   "atiny"
	   :summary
	   (concat "asm tiny")))
	#("small" 0 1
	  (:annotation
	   "asmall"
	   :summary
	   (concat "asm small")))
	#("medium" 0 1
	  (:annotation
	   "amedium"
	   :summary
	   (concat "asm medium")))
	#("compact" 0 1
	  (:annotation
	   "acompact"
	   :summary
	   (concat "asm acompact")))
	#("large" 0 1
	  (:annotation
	   "alarge"
	   :summary
	   (concat "asm large")))
	#("huge" 0 1
	  (:annotation
	   "ahuge"
	   :summary
	   (concat "asm huge")))
	;; eax
	#("al" 0 1
      (:annotation
	   "aal"
	   :summary
	   (concat "asm al")))
    #("ah" 0 1
      (:annotation
	   "aah"
	   :summary
	   (concat "asm ah")))
	#("ax" 0 1
      (:annotation
	   "aax"
	   :summary
	   (concat "asm ax")))
	#("eax" 0 1
      (:annotation
	   "aeax"
	   :summary
	   (concat "asm eax")))
	;; ebx
	#("bl" 0 1
      (:annotation
	   "abl"
	   :summary
	   (concat "asm bl")))
    #("bh" 0 1
      (:annotation
	   "abh"
	   :summary
	   (concat "asm bh")))
	#("bx" 0 1
      (:annotation
	   "abx"
	   :summary
	   (concat "asm bx")))
	#("ebx" 0 1
      (:annotation
	   "aebx"
	   :summary
	   (concat "asm ebx")))
	;; ecx
	#("cl" 0 1
      (:annotation
	   "acl"
	   :summary
	   (concat "asm cl")))
    #("ch" 0 1
      (:annotation
	   "ach"
	   :summary
	   (concat "asm ch")))
	#("cx" 0 1
      (:annotation
	   "acx"
	   :summary
	   (concat "asm cx")))
	#("ecx" 0 1
      (:annotation
	   "aecx"
	   :summary
	   (concat "asm ecx")))

	;; edx
	#("dl" 0 1
      (:annotation
	   "adl"
	   :summary
	   (concat "asm dl")))
    #("dh" 0 1
      (:annotation
	   "adh"
	   :summary
	   (concat "asm dh")))
	#("dx" 0 1
      (:annotation
	   "adx"
	   :summary
	   (concat "asm dx")))
	#("edx" 0 1
      (:annotation
	   "aedx"
	   :summary
	   (concat "asm edx")))

	;; esi
	#("si" 0 1
      (:annotation
	   "asi"
	   :summary
	   (concat "asm si")))
	#("esi" 0 1
      (:annotation
	   "aesi"
	   :summary
	   (concat "asm esi")))

	;; edi
	#("di" 0 1
      (:annotation
	   "adi"
	   :summary
	   (concat "asm di")))
	#("edi" 0 1
      (:annotation
	   "aedi"
	   :summary
	   (concat "asm edi")))

	;; ebp
	#("bp" 0 1
      (:annotation
	   "abp"
	   :summary
	   (concat "asm bp")))
	#("ebp" 0 1
      (:annotation
	   "aebp"
	   :summary
	   (concat "asm ebp")))

	;; esp
	#("sp" 0 1
      (:annotation
	   "asp"
	   :summary
	   (concat "asm sp")))
	#("esp" 0 1
      (:annotation
	   "aesp"
	   :summary
	   (concat "asm esp")))

	;;
	#("cs" 0 1
      (:annotation
	   "acs"
	   :summary
	   (concat "asm ecs")))
	#("ds" 0 1
      (:annotation
	   "ads"
	   :summary
	   (concat "asm ds")))
	#("ss" 0 1
      (:annotation
	   "ass"
	   :summary
	   (concat "asm ss")))
	#("es" 0 1
      (:annotation
	   "aes"
	   :summary
	   (concat "asm es")))
	#("fs" 0 1
      (:annotation
	   "afs"
	   :summary
	   (concat "asm fs")))
	#("gs" 0 1
      (:annotation
	   "ags"
	   :summary
	   (concat "asm gs")))

	;; 64
	#("r0" 0 1
      (:annotation
	   "ar0"
	   :summary
	   (concat "asm r0")))
	#("r1" 0 1
      (:annotation
	   "ar1"
	   :summary
	   (concat "asm r1")))
	#("r2" 0 1
      (:annotation
	   "ar2"
	   :summary
	   (concat "asm r2")))
	#("r3" 0 1
      (:annotation
	   "ar3"
	   :summary
	   (concat "asm r3")))
	#("r4" 0 1
      (:annotation
	   "r4"
	   :summary
	   (concat "asm r4")))
	#("r5" 0 1
      (:annotation
	   "ar5"
	   :summary
	   (concat "asm r5")))
	#("r6" 0 1
      (:annotation
	   "ar6"
	   :summary
	   (concat "asm r6")))
	#("r7" 0 1
      (:annotation
	   "ar7"
	   :summary
	   (concat "asm r7")))

	;; mmx
	#("mm0" 0 1
      (:annotation
	   "amm0"
	   :summary
	   (concat "asm mm0")))
	#("mm1" 0 1
      (:annotation
	   "amm1"
	   :summary
	   (concat "asm mm1")))
	#("mm2" 0 1
      (:annotation
	   "amm2"
	   :summary
	   (concat "asm mm2")))
	#("mm3" 0 1
      (:annotation
	   "amm3"
	   :summary
	   (concat "asm mm3")))
	#("mm4" 0 1
      (:annotation
	   "mm4"
	   :summary
	   (concat "asm mm4")))
	#("mm5" 0 1
      (:annotation
	   "amm5"
	   :summary
	   (concat "asm mm5")))
	#("mm6" 0 1
      (:annotation
	   "amm6"
	   :summary
	   (concat "asm mm6")))
	#("mm7" 0 1
      (:annotation
	   "amm7"
	   :summary
	   (concat "asm mm7")))

	;; ymmx
	#("ymm0" 0 1
      (:annotation
	   "aymm0"
	   :summary
	   (concat "asm ymm0")))
	#("ymm1" 0 1
      (:annotation
	   "aymm1"
	   :summary
	   (concat "asm ymm1")))
	#("ymm2" 0 1
      (:annotation
	   "aymm2"
	   :summary
	   (concat "asm ymm2")))
	#("ymm3" 0 1
      (:annotation
	   "aymm3"
	   :summary
	   (concat "asm ymm3")))
	#("ymm4" 0 1
      (:annotation
	   "aymm4"
	   :summary
	   (concat "asm ymm4")))
	#("ymm5" 0 1
      (:annotation
	   "aymm5"
	   :summary
	   (concat "asm ymm5")))
	#("ymm6" 0 1
      (:annotation
	   "aymm6"
	   :summary
	   (concat "asm ymm6")))
	#("ymm7" 0 1
      (:annotation
	   "aymm7"
	   :summary
	   (concat "asm ymm7")))

	;; xmmx
	#("xmm0" 0 1
      (:annotation
	   "axmm0"
	   :summary
	   (concat "asm xmm0")))
	#("xmm1" 0 1
      (:annotation
	   "axmm1"
	   :summary
	   (concat "asm xmm1")))
	#("xmm2" 0 1
      (:annotation
	   "axmm2"
	   :summary
	   (concat "asm xmm2")))
	#("xmm3" 0 1
      (:annotation
	   "axmm3"
	   :summary
	   (concat "asm xmm3")))
	#("xmm4" 0 1
      (:annotation
	   "axmm4"
	   :summary
	   (concat "asm xmm4")))
	#("xmm5" 0 1
      (:annotation
	   "axmm5"
	   :summary
	   (concat "asm xmm5")))
	#("xmm6" 0 1
      (:annotation
	   "axmm6"
	   :summary
	   (concat "asm xmm6")))
	#("xmm7" 0 1
      (:annotation
	   "axmm7"
	   :summary
	   (concat "asm xmm7")))

	;; eflags
	#("eflags" 0 1
      (:annotation
	   "aeflags"
	   :summary
	   (concat "asm eflags")))
	#("cf" 0 1
      (:annotation
	   "afcf"
	   :summary
	   (concat "asm eflags cf")))
	#("pf" 0 1
      (:annotation
	   "afpf"
	   :summary
	   (concat "asm eflags pf")))
	#("af" 0 1
      (:annotation
	   "afaf"
	   :summary
	   (concat "asm eflags af")))
	#("zf" 0 1
      (:annotation
	   "afzf"
	   :summary
	   (concat "asm eflags zf")))
	#("sf" 0 1
      (:annotation
	   "afsf"
	   :summary
	   (concat "asm eflags sf")))
	#("tf" 0 1
      (:annotation
	   "aftf"
	   :summary
	   (concat "asm eflags tf")))
	#("if" 0 1
      (:annotation
	   "afif"
	   :summary
	   (concat "asm eflags if")))
	#("df" 0 1
      (:annotation
	   "afdf"
	   :summary
	   (concat "asm eflags df")))
	#("of" 0 1
      (:annotation
	   "afof"
	   :summary
	   (concat "asm eflags of")))
	#("iopl" 0 1
      (:annotation
	   "afiopl"
	   :summary
	   (concat "asm eflags iopl")))
	#("nt" 0 1
      (:annotation
	   "afnt"
	   :summary
	   (concat "asm eflags nt")))
	#("rf" 0 1
      (:annotation
	   "afrf"
	   :summary
	   (concat "asm eflags rf")))
	#("vm" 0 1
      (:annotation
	   "afvm"
	   :summary
	   (concat "asm eflags vm")))
	#("ac" 0 1
      (:annotation
	   "afac"
	   :summary
	   (concat "asm eflags ac")))
	#("cf" 0 1
      (:annotation
	   "afcf"
	   :summary
	   (concat "asm eflags cf")))
	#("vif" 0 1
      (:annotation
	   "afvif"
	   :summary
	   (concat "asm eflags vif")))
	#("vip" 0 1
      (:annotation
	   "afvip"
	   :summary
	   (concat "asm eflags vip")))
	#("id" 0 1
      (:annotation
	   "afid"
	   :summary
	   (concat "asm eflags id")))

	;; eip
	#("eip" 0 1
      (:annotation
	   "aeip"
	   :summary
	   (concat "asm eip")))

	;; data
	#("mov" 0 1
      (:annotation
	   "amov"
	   :summary
	   (concat "asm mov")))
	#("cmovcc" 0 1
      (:annotation
	   "acmovcc"
	   :summary
	   (concat "asm cmovcc")))
	#("push" 0 1
      (:annotation
	   "apush"
	   :summary
	   (concat "asm push")))
	#("pop" 0 1
      (:annotation
	   "apop"
	   :summary
	   (concat "asm pop")))
	#("pushad" 0 1
      (:annotation
	   "apushad"
	   :summary
	   (concat "asm pushad")))
	#("xchg" 0 1
      (:annotation
	   "axchg"
	   :summary
	   (concat "asm xchg")))
	#("xadd" 0 1
      (:annotation
	   "axadd"
	   :summary
	   (concat "asm xadd")))
	#("movsx" 0 1
      (:annotation
	   "amovsx"
	   :summary
	   (concat "asm movsx")))
	#("movzx" 0 1
      (:annotation
	   "amovzx"
	   :summary
	   (concat "asm movzx")))

	;; bineray
	#("add" 0 1
      (:annotation
	   "aadd"
	   :summary
	   (concat "asm add")))
	#("adc" 0 1
      (:annotation
	   "aadc"
	   :summary
	   (concat "asm adc")))
	#("sub" 0 1
      (:annotation
	   "asub"
	   :summary
	   (concat "asm sub")))
	#("sbb" 0 1
      (:annotation
	   "asbb"
	   :summary
	   (concat "asm sbb")))
	#("imul" 0 1
      (:annotation
	   "aimul"
	   :summary
	   (concat "asm imul")))
	#("mul" 0 1
      (:annotation
	   "amul"
	   :summary
	   (concat "asm mul")))
	#("idiv" 0 1
      (:annotation
	   "aidiv"
	   :summary
	   (concat "asm idiv")))
	#("div" 0 1
      (:annotation
	   "adiv"
	   :summary
	   (concat "asm div")))
	#("inc" 0 1
      (:annotation
	   "ainc"
	   :summary
	   (concat "asm inc")))
	#("dec" 0 1
      (:annotation
	   "adec"
	   :summary
	   (concat "asm dec")))
	#("neg" 0 1
      (:annotation
	   "aneg"
	   :summary
	   (concat "asm neg")))
	#("daa" 0 1
      (:annotation
	   "adaa"
	   :summary
	   (concat "asm daa")))
	#("das" 0 1
      (:annotation
	   "adas"
	   :summary
	   (concat "asm das")))
	#("aaa" 0 1
      (:annotation
	   "aaaa"
	   :summary
	   (concat "asm aaa")))
	#("aas" 0 1
      (:annotation
	   "aaas"
	   :summary
	   (concat "asm aas")))
	#("aam" 0 1
      (:annotation
	   "aaam"
	   :summary
	   (concat "asm aam")))
	#("aad" 0 1
      (:annotation
	   "aaad"
	   :summary
	   (concat "asm aad")))
	#("inc" 0 1
      (:annotation
	   "ainc"
	   :summary
	   (concat "asm inc")))

	;; data compare
	#("cmp" 0 1
      (:annotation
	   "acmp"
	   :summary
	   (concat "asm cmp")))
	#("cmpxchg" 0 1
      (:annotation
	   "acmpxchg"
	   :summary
	   (concat "asm cmpxchg")))
	#("cmpxchg8b" 0 1
      (:annotation
	   "acmpxchg8b"
	   :summary
	   (concat "asm cmpxchg8b")))

	;; data convert
	#("cbw" 0 1
      (:annotation
	   "acbw"
	   :summary
	   (concat "asm cbw")))
	#("cwde" 0 1
      (:annotation
	   "acwde"
	   :summary
	   (concat "asm cwde")))
	#("cwd" 0 1
      (:annotation
	   "acwd"
	   :summary
	   (concat "asm cwd")))
	#("cdq" 0 1
      (:annotation
	   "acdq"
	   :summary
	   (concat "asm cdq")))
	#("bswap" 0 1
      (:annotation
	   "abswap"
	   :summary
	   (concat "asm bswap")))
	#("movbe" 0 1
      (:annotation
	   "amovbe"
	   :summary
	   (concat "asm movbe")))
	#("xlatb" 0 1
      (:annotation
	   "axlatb"
	   :summary
	   (concat "asm xlatb")))

	;; Logical operation
	#("and" 0 1
      (:annotation
	   "aand"
	   :summary
	   (concat "asm aand")))
	#("or" 0 1
      (:annotation
	   "aor"
	   :summary
	   (concat "asm or")))
	#("xor" 0 1
      (:annotation
	   "axor"
	   :summary
	   (concat "asm xor")))
	#("not" 0 1
      (:annotation
	   "anot"
	   :summary
	   (concat "asm anot")))
	#("test" 0 1
      (:annotation
	   "atest"
	   :summary
	   (concat "asm test")))

	;; displacement
	#("rcl" 0 1
      (:annotation
	   "arcl"
	   :summary
	   (concat "asm rcl")))
	#("rcr" 0 1
      (:annotation
	   "arcr"
	   :summary
	   (concat "asm rcr")))
	#("rol" 0 1
      (:annotation
	   "arol"
	   :summary
	   (concat "asm rol")))
	#("ror" 0 1
      (:annotation
	   "aror"
	   :summary
	   (concat "asm ror")))
	#("sal" 0 1
      (:annotation
	   "asal"
	   :summary
	   (concat "asm sal")))
	#("shl" 0 1
      (:annotation
	   "ashl"
	   :summary
	   (concat "asm ashl")))
	#("sar" 0 1
      (:annotation
	   "asar"
	   :summary
	   (concat "asm sar")))
	#("shr" 0 1
      (:annotation
	   "ashr"
	   :summary
	   (concat "asm shr")))
	#("shld" 0 1
      (:annotation
	   "ashld"
	   :summary
	   (concat "asm shld")))
	#("shrd" 0 1
      (:annotation
	   "ashrd"
	   :summary
	   (concat "asm shrd")))

	;; bit
	#("setcc" 0 1
      (:annotation
	   "asetcc"
	   :summary
	   (concat "asm setcc")))
	#("bt" 0 1
      (:annotation
	   "abt"
	   :summary
	   (concat "asm bt")))
	#("bts" 0 1
      (:annotation
	   "abts"
	   :summary
	   (concat "asm bts")))
	#("btr" 0 1
      (:annotation
	   "abtr"
	   :summary
	   (concat "asm btr")))
	#("btc" 0 1
      (:annotation
	   "abtc"
	   :summary
	   (concat "asm btc")))
	#("bsf" 0 1
      (:annotation
	   "absf"
	   :summary
	   (concat "asm bsf")))
	#("bsr" 0 1
      (:annotation
	   "absr"
	   :summary
	   (concat "asm bsr")))
	#("test" 0 1
      (:annotation
	   "atest"
	   :summary
	   (concat "asm test")))

	;; string
	#("cmpsb" 0 1
      (:annotation
	   "acmpsb"
	   :summary
	   (concat "asm cmpsb")))
	#("cmpsw" 0 1
      (:annotation
	   "acmpsw"
	   :summary
	   (concat "asm cmpsw")))
	#("cmpsd" 0 1
      (:annotation
	   "acmpsd"
	   :summary
	   (concat "asm cmpsd")))
	#("lodsb" 0 1
      (:annotation
	   "alodsb"
	   :summary
	   (concat "asm lodsb")))
	#("lodsw" 0 1
      (:annotation
	   "alodsw"
	   :summary
	   (concat "asm lodsw")))
	#("lodsd" 0 1
      (:annotation
	   "alodsd"
	   :summary
	   (concat "asm lodsd")))
	#("movsb" 0 1
      (:annotation
	   "amovsb"
	   :summary
	   (concat "asm movsb")))
	#("movsw" 0 1
      (:annotation
	   "amovsw"
	   :summary
	   (concat "asm movsw")))
	#("movsd" 0 1
      (:annotation
	   "amovsd"
	   :summary
	   (concat "asm movsd")))
	#("scasb" 0 1
      (:annotation
	   "ascasb"
	   :summary
	   (concat "asm scasb")))
	#("scasw" 0 1
      (:annotation
	   "ascasw"
	   :summary
	   (concat "asm scasw")))
	#("scasd" 0 1
      (:annotation
	   "ascasd"
	   :summary
	   (concat "asm scasd")))
	#("stosb" 0 1
      (:annotation
	   "astosb"
	   :summary
	   (concat "asm stosb")))
	#("stosw" 0 1
      (:annotation
	   "astosw"
	   :summary
	   (concat "asm stosw")))
	#("stosd" 0 1
      (:annotation
	   "astosd"
	   :summary
	   (concat "asm stosd")))
	#("rep" 0 1
      (:annotation
	   "arep"
	   :summary
	   (concat "asm rep")))
	#("repe" 0 1
      (:annotation
	   "arepe"
	   :summary
	   (concat "asm repe")))
	#("repz" 0 1
      (:annotation
	   "arepz"
	   :summary
	   (concat "asm repz")))
	#("repne" 0 1
      (:annotation
	   "arepne"
	   :summary
	   (concat "asm repne")))
	#("repnz" 0 1
      (:annotation
	   "arepnz"
	   :summary
	   (concat "asm repnz")))

	;; flags oprion
	#("clc" 0 1
      (:annotation
	   "aclc"
	   :summary
	   (concat "asm clc")))
	#("stc" 0 1
      (:annotation
	   "astc"
	   :summary
	   (concat "asm stc")))
	#("cmc" 0 1
      (:annotation
	   "acmc"
	   :summary
	   (concat "asm cmc")))
	#("std" 0 1
      (:annotation
	   "astd"
	   :summary
	   (concat "asm std")))
	#("cld" 0 1
      (:annotation
	   "acld"
	   :summary
	   (concat "asm cld")))
	#("lahf" 0 1
      (:annotation
	   "alahf"
	   :summary
	   (concat "asm lahf")))
	#("sahf" 0 1
      (:annotation
	   "sahf"
	   :summary
	   (concat "asm sahf")))
	#("pushfd" 0 1
      (:annotation
	   "apushfd"
	   :summary
	   (concat "asm pushfd")))
	#("popfd" 0 1
      (:annotation
	   "apopfd"
	   :summary
	   (concat "asm popfd")))

	;; control
	#("jmp" 0 1
      (:annotation
	   "ajmp"
	   :summary
	   (concat "asm jmp")))
	#("jcc" 0 1
      (:annotation
	   "ajcc"
	   :summary
	   (concat "asm jcc")))
	#("call" 0 1
      (:annotation
	   "acall"
	   :summary
	   (concat "asm call")))
	#("ret" 0 1
      (:annotation
	   "aret"
	   :summary
	   (concat "asm ret")))
	#("enter" 0 1
      (:annotation
	   "aenter"
	   :summary
	   (concat "asm enter")))
	#("leave" 0 1
      (:annotation
	   "aleave"
	   :summary
	   (concat "asm leave")))
	#("jecxz" 0 1
      (:annotation
	   "ajecxz"
	   :summary
	   (concat "asm jecxz")))
	#("loop" 0 1
      (:annotation
	   "aloop"
	   :summary
	   (concat "asm loop")))
	#("loope" 0 1
      (:annotation
	   "aloope"
	   :summary
	   (concat "asm loope")))
	#("loopz" 0 1
      (:annotation
	   "aloopz"
	   :summary
	   (concat "asm loopz")))
	#("loopne" 0 1
      (:annotation
	   "aloopne"
	   :summary
	   (concat "asm loopne")))
	#("loopnz" 0 1
      (:annotation
	   "aloopnz"
	   :summary
	   (concat "asm loopnz")))

	;; other
	#("bound" 0 1
      (:annotation
	   "abound"
	   :summary
	   (concat "asm bound")))
	#("lea" 0 1
      (:annotation
	   "alea"
	   :summary
	   (concat "asm lea")))
	#("nop" 0 1
      (:annotation
	   "anop"
	   :summary
	   (concat "asm nop")))
	#("cupid" 0 1
      (:annotation
	   "acupid"
	   :summary
	   (concat "asm cupid"))))
  "Assembly company completions.")

(defun company-asm--completions ()
  "Get completions."
  (let* ((completions company-asm--static-completions))
	completions))

(defun company-asm--prefix ()
  "Get prefix."
  (and (or (eq major-mode 'asm-mode) (eq major-mode 'nasm-mode))
	   (company-grab-symbol)))

(defun company-asm--matching-condidates (arg)
  "Get ARG for fuzzy matching condidates."
  (let* ((condidates))
	(setq condidates
		  (remove-if-not
		   (lambda (candidate)
			 (company-asm--fuzzy-match arg candidate))
		   (company-asm--completions)))
	condidates))

(defun company-asm--annotation (annotation)
  "Get ANNOTATION."
  (format " %s" (get-text-property 0 :annotation annotation)))

(defun company-asm--summary (summary)
  "Get SUMMARY."
  (format " %s" (get-text-property 0 :summary summary)))

(defun company-asm--fuzzy-match (prefix candidate)
  "Fuzzy match CANDIDATE by PREFIX."
  (cl-subsetp (string-to-list prefix)
              (string-to-list candidate)))

(defun company-assembly (command &optional arg &rest ignored)
  "Company assembly backend.
COMMAND can take any of a number of values.
the ARG argument holds the value returned by prefix.
IGNORED list."
  (interactive (list 'interactive))
  (case command
	(interactive (company-begin-backend 'company-assembly))
	(prefix (or (company-asm--prefix) 'stop))
	(candidates (company-asm--matching-condidates arg))
	(annotation (company-asm--annotation arg))
	(summary (company-asm--summary arg))
	(no-cache 't)))

(provide 'company-asm)
;;; company-asm.el ends here
