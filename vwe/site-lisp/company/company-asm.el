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

(require 'cl-lib)
(require 'company)

(defconst company-asm--manual-asm-completions
  '(;; Macro Directive
	#("section" 0 1
	  (:initials
	   "asection"
	   :summary
	   (concat "asm section")))
	#("global" 0 1
	  (:initials
	   "aglobal"
	   :summary
	   (concat "asm global")))
	#(".text" 0 1
	  (:initials
	   "adottext"
	   :summary
	   (concat "asm .text")))
	#(".code" 0 1
	  (:initials
	   "adotcode"
	   :summary
	   (concat "asm .code")))
	#(".data" 0 1
	  (:initials
	   "adotdata"
	   :summary
	   (concat "asm .data")))
	#(".data?" 0 1
	  (:initials
	   "adotdata?"
	   :summary
	   (concat "asm .data?")))
	#(".const" 0 1
	  (:initials
	   "adotconst"
	   :summary
	   (concat "asm .const")))
	#(".fardata" 0 1
	  (:initials
	   "adotfardata"
	   :summary
	   (concat "asm .fardata")))
	#(".fardata?" 0 1
	  (:initials
	   "adotfardata?"
	   :summary
	   (concat "asm .fardata?")))
	#(".stack" 0 1
	  (:initials
	   "adotstack"
	   :summary
	   (concat "asm .stack")))
	#("@model" 0 1
	  (:initials
	   "a@model"
	   :summary
	   (concat "asm @model")))
	#("@code" 0 1
	  (:initials
	   "a@code"
	   :summary
	   (concat "asm @code")))
	#("@data" 0 1
	  (:initials
	   "a@data"
	   :summary
	   (concat "asm @data")))
	#("@fardata" 0 1
	  (:initials
	   "a@fardata"
	   :summary
	   (concat "asm @fardata")))
	#("@stack" 0 1
	  (:initials
	   "a@stack"
	   :summary
	   (concat "asm @stack")))
	#("@codesize" 0 1
	  (:initials
	   "a@codesize"
	   :summary
	   (concat "asm @codesize")))
	#("@datasize" 0 1
	  (:initials
	   "a@datasize"
	   :summary
	   (concat "asm @datasize")))
	#("org" 0 1
	  (:initials
	   "aorg"
	   :summary
	   (concat "asm org")))
	#("even" 0 1
	  (:initials
	   "aeven"
	   :summary
	   (concat "asm even")))
	#("proc" 0 1
	  (:initials
	   "aproc"
	   :summary
	   (concat "asm proc")))
	#("near" 0 1
	  (:initials
	   "anear"
	   :summary
	   (concat "asm near")))
	#("far" 0 1
	  (:initials
	   "afar"
	   :summary
	   (concat "asm far")))
	#("include" 0 1
	  (:initials
	   "ainclude"
	   :summary
	   (concat "asm include")))
	#("title" 0 1
	  (:initials
	   "atitle"
	   :summary
	   (concat "asm title")))
	#("macro" 0 1
	  (:initials
	   "amacro"
	   :summary
	   (concat "asm macro")))
	#("local" 0 1
	  (:initials
	   "alocal"
	   :summary
	   (concat "asm local")))
	#("extra" 0 1
	  (:initials
	   "aextra"
	   :summary
	   (concat "asm extra")))
	#("stack" 0 1
	  (:initials
	   "astack"
	   :summary
	   (concat "asm stack")))
	#("being" 0 1
	  (:initials
	   "abeing"
	   :summary
	   (concat "asm being")))
	#("code" 0 1
	  (:initials
	   "acode"
	   :summary
	   (concat "asm code")))
	#("param" 0 1
	  (:initials
	   "aparam"
	   :summary
	   (concat "asm param")))
	#("_start" 0 1
	  (:initials
	   "astart"
	   :summary
	   (concat "asm _start")))
	#("segment" 0 1
	  (:initials
	   "asegment"
	   :summary
	   (concat "asm segemnt")))
	#("end" 0 1
	  (:initials
	   "aend"
	   :summary
	   (concat "asm end")))
	#("ends" 0 1
	  (:initials
	   "aends"
	   :summary
	   (concat "asm ends")))
	#("exit" 0 1
	  (:initials
	   "aexit"
	   :summary
	   (concat "asm exit")))
	#("int" 0 1
	  (:initials
	   "aint"
	   :summary
	   (concat "asm int")))
	#("assume" 0 1
	  (:initials
	   "aassume"
	   :summary
	   (concat "asm assume")))
	#("nothing" 0 1
	  (:initials
	   "anothing"
	   :summary
	   (concat "asm nothing")))
	#("group" 0 1
	  (:initials
	   "agroup"
	   :summary
	   (concat "asm group")))
	#("model" 0 1
	  (:initials
	   "amodel"
	   :summary
	   (concat "asm model")))
	#("tiny" 0 1
	  (:initials
	   "atiny"
	   :summary
	   (concat "asm tiny")))
	#("small" 0 1
	  (:initials
	   "asmall"
	   :summary
	   (concat "asm small")))
	#("medium" 0 1
	  (:initials
	   "amedium"
	   :summary
	   (concat "asm medium")))
	#("compact" 0 1
	  (:initials
	   "acompact"
	   :summary
	   (concat "asm acompact")))
	#("large" 0 1
	  (:initials
	   "alarge"
	   :summary
	   (concat "asm large")))
	#("huge" 0 1
	  (:initials
	   "ahuge"
	   :summary
	   (concat "asm huge")))
	;; eax
	#("al" 0 1
      (:initials
	   "aal"
	   :summary
	   (concat "asm al")))
    #("ah" 0 1
      (:initials
	   "aah"
	   :summary
	   (concat "asm ah")))
	#("ax" 0 1
      (:initials
	   "aax"
	   :summary
	   (concat "asm ax")))
	#("eax" 0 1
      (:initials
	   "aeax"
	   :summary
	   (concat "asm eax")))
	;; ebx
	#("bl" 0 1
      (:initials
	   "abl"
	   :summary
	   (concat "asm bl")))
    #("bh" 0 1
      (:initials
	   "abh"
	   :summary
	   (concat "asm bh")))
	#("bx" 0 1
      (:initials
	   "abx"
	   :summary
	   (concat "asm bx")))
	#("ebx" 0 1
      (:initials
	   "aebx"
	   :summary
	   (concat "asm ebx")))
	;; ecx
	#("cl" 0 1
      (:initials
	   "acl"
	   :summary
	   (concat "asm cl")))
    #("ch" 0 1
      (:initials
	   "ach"
	   :summary
	   (concat "asm ch")))
	#("cx" 0 1
      (:initials
	   "acx"
	   :summary
	   (concat "asm cx")))
	#("ecx" 0 1
      (:initials
	   "aecx"
	   :summary
	   (concat "asm ecx")))

	;; edx
	#("dl" 0 1
      (:initials
	   "adl"
	   :summary
	   (concat "asm dl")))
    #("dh" 0 1
      (:initials
	   "adh"
	   :summary
	   (concat "asm dh")))
	#("dx" 0 1
      (:initials
	   "adx"
	   :summary
	   (concat "asm dx")))
	#("edx" 0 1
      (:initials
	   "aedx"
	   :summary
	   (concat "asm edx")))

	;; esi
	#("si" 0 1
      (:initials
	   "asi"
	   :summary
	   (concat "asm si")))
	#("esi" 0 1
      (:initials
	   "aesi"
	   :summary
	   (concat "asm esi")))

	;; edi
	#("di" 0 1
      (:initials
	   "adi"
	   :summary
	   (concat "asm di")))
	#("edi" 0 1
      (:initials
	   "aedi"
	   :summary
	   (concat "asm edi")))

	;; ebp
	#("bp" 0 1
      (:initials
	   "abp"
	   :summary
	   (concat "asm bp")))
	#("ebp" 0 1
      (:initials
	   "aebp"
	   :summary
	   (concat "asm ebp")))

	;; esp
	#("sp" 0 1
      (:initials
	   "asp"
	   :summary
	   (concat "asm sp")))
	#("esp" 0 1
      (:initials
	   "aesp"
	   :summary
	   (concat "asm esp")))

	;;
	#("cs" 0 1
      (:initials
	   "acs"
	   :summary
	   (concat "asm ecs")))
	#("ds" 0 1
      (:initials
	   "ads"
	   :summary
	   (concat "asm ds")))
	#("ss" 0 1
      (:initials
	   "ass"
	   :summary
	   (concat "asm ss")))
	#("es" 0 1
      (:initials
	   "aes"
	   :summary
	   (concat "asm es")))
	#("fs" 0 1
      (:initials
	   "afs"
	   :summary
	   (concat "asm fs")))
	#("gs" 0 1
      (:initials
	   "ags"
	   :summary
	   (concat "asm gs")))

	;; 64
	#("r0" 0 1
      (:initials
	   "ar0"
	   :summary
	   (concat "asm r0")))
	#("r1" 0 1
      (:initials
	   "ar1"
	   :summary
	   (concat "asm r1")))
	#("r2" 0 1
      (:initials
	   "ar2"
	   :summary
	   (concat "asm r2")))
	#("r3" 0 1
      (:initials
	   "ar3"
	   :summary
	   (concat "asm r3")))
	#("r4" 0 1
      (:initials
	   "r4"
	   :summary
	   (concat "asm r4")))
	#("r5" 0 1
      (:initials
	   "ar5"
	   :summary
	   (concat "asm r5")))
	#("r6" 0 1
      (:initials
	   "ar6"
	   :summary
	   (concat "asm r6")))
	#("r7" 0 1
      (:initials
	   "ar7"
	   :summary
	   (concat "asm r7")))

	;; mmx
	#("mm0" 0 1
      (:initials
	   "amm0"
	   :summary
	   (concat "asm mm0")))
	#("mm1" 0 1
      (:initials
	   "amm1"
	   :summary
	   (concat "asm mm1")))
	#("mm2" 0 1
      (:initials
	   "amm2"
	   :summary
	   (concat "asm mm2")))
	#("mm3" 0 1
      (:initials
	   "amm3"
	   :summary
	   (concat "asm mm3")))
	#("mm4" 0 1
      (:initials
	   "mm4"
	   :summary
	   (concat "asm mm4")))
	#("mm5" 0 1
      (:initials
	   "amm5"
	   :summary
	   (concat "asm mm5")))
	#("mm6" 0 1
      (:initials
	   "amm6"
	   :summary
	   (concat "asm mm6")))
	#("mm7" 0 1
      (:initials
	   "amm7"
	   :summary
	   (concat "asm mm7")))

	;; ymmx
	#("ymm0" 0 1
      (:initials
	   "aymm0"
	   :summary
	   (concat "asm ymm0")))
	#("ymm1" 0 1
      (:initials
	   "aymm1"
	   :summary
	   (concat "asm ymm1")))
	#("ymm2" 0 1
      (:initials
	   "aymm2"
	   :summary
	   (concat "asm ymm2")))
	#("ymm3" 0 1
      (:initials
	   "aymm3"
	   :summary
	   (concat "asm ymm3")))
	#("ymm4" 0 1
      (:initials
	   "aymm4"
	   :summary
	   (concat "asm ymm4")))
	#("ymm5" 0 1
      (:initials
	   "aymm5"
	   :summary
	   (concat "asm ymm5")))
	#("ymm6" 0 1
      (:initials
	   "aymm6"
	   :summary
	   (concat "asm ymm6")))
	#("ymm7" 0 1
      (:initials
	   "aymm7"
	   :summary
	   (concat "asm ymm7")))

	;; xmmx
	#("xmm0" 0 1
      (:initials
	   "axmm0"
	   :summary
	   (concat "asm xmm0")))
	#("xmm1" 0 1
      (:initials
	   "axmm1"
	   :summary
	   (concat "asm xmm1")))
	#("xmm2" 0 1
      (:initials
	   "axmm2"
	   :summary
	   (concat "asm xmm2")))
	#("xmm3" 0 1
      (:initials
	   "axmm3"
	   :summary
	   (concat "asm xmm3")))
	#("xmm4" 0 1
      (:initials
	   "axmm4"
	   :summary
	   (concat "asm xmm4")))
	#("xmm5" 0 1
      (:initials
	   "axmm5"
	   :summary
	   (concat "asm xmm5")))
	#("xmm6" 0 1
      (:initials
	   "axmm6"
	   :summary
	   (concat "asm xmm6")))
	#("xmm7" 0 1
      (:initials
	   "axmm7"
	   :summary
	   (concat "asm xmm7")))

	;; eflags
	#("eflags" 0 1
      (:initials
	   "aeflags"
	   :summary
	   (concat "asm eflags")))
	#("cf" 0 1
      (:initials
	   "afcf"
	   :summary
	   (concat "asm eflags cf")))
	#("pf" 0 1
      (:initials
	   "afpf"
	   :summary
	   (concat "asm eflags pf")))
	#("af" 0 1
      (:initials
	   "afaf"
	   :summary
	   (concat "asm eflags af")))
	#("zf" 0 1
      (:initials
	   "afzf"
	   :summary
	   (concat "asm eflags zf")))
	#("sf" 0 1
      (:initials
	   "afsf"
	   :summary
	   (concat "asm eflags sf")))
	#("tf" 0 1
      (:initials
	   "aftf"
	   :summary
	   (concat "asm eflags tf")))
	#("if" 0 1
      (:initials
	   "afif"
	   :summary
	   (concat "asm eflags if")))
	#("df" 0 1
      (:initials
	   "afdf"
	   :summary
	   (concat "asm eflags df")))
	#("of" 0 1
      (:initials
	   "afof"
	   :summary
	   (concat "asm eflags of")))
	#("iopl" 0 1
      (:initials
	   "afiopl"
	   :summary
	   (concat "asm eflags iopl")))
	#("nt" 0 1
      (:initials
	   "afnt"
	   :summary
	   (concat "asm eflags nt")))
	#("rf" 0 1
      (:initials
	   "afrf"
	   :summary
	   (concat "asm eflags rf")))
	#("vm" 0 1
      (:initials
	   "afvm"
	   :summary
	   (concat "asm eflags vm")))
	#("ac" 0 1
      (:initials
	   "afac"
	   :summary
	   (concat "asm eflags ac")))
	#("cf" 0 1
      (:initials
	   "afcf"
	   :summary
	   (concat "asm eflags cf")))
	#("vif" 0 1
      (:initials
	   "afvif"
	   :summary
	   (concat "asm eflags vif")))
	#("vip" 0 1
      (:initials
	   "afvip"
	   :summary
	   (concat "asm eflags vip")))
	#("id" 0 1
      (:initials
	   "afid"
	   :summary
	   (concat "asm eflags id")))

	;; eip
	#("eip" 0 1
      (:initials
	   "aeip"
	   :summary
	   (concat "asm eip")))

	;; data
	#("mov" 0 1
      (:initials
	   "amov"
	   :summary
	   (concat "asm mov")))
	#("cmovcc" 0 1
      (:initials
	   "acmovcc"
	   :summary
	   (concat "asm cmovcc")))
	#("push" 0 1
      (:initials
	   "apush"
	   :summary
	   (concat "asm push")))
	#("pop" 0 1
      (:initials
	   "apop"
	   :summary
	   (concat "asm pop")))
	#("pushad" 0 1
      (:initials
	   "apushad"
	   :summary
	   (concat "asm pushad")))
	#("xchg" 0 1
      (:initials
	   "axchg"
	   :summary
	   (concat "asm xchg")))
	#("xadd" 0 1
      (:initials
	   "axadd"
	   :summary
	   (concat "asm xadd")))
	#("movsx" 0 1
      (:initials
	   "amovsx"
	   :summary
	   (concat "asm movsx")))
	#("movzx" 0 1
      (:initials
	   "amovzx"
	   :summary
	   (concat "asm movzx")))

	;; bineray
	#("add" 0 1
      (:initials
	   "aadd"
	   :summary
	   (concat "asm add")))
	#("adc" 0 1
      (:initials
	   "aadc"
	   :summary
	   (concat "asm adc")))
	#("sub" 0 1
      (:initials
	   "asub"
	   :summary
	   (concat "asm sub")))
	#("sbb" 0 1
      (:initials
	   "asbb"
	   :summary
	   (concat "asm sbb")))
	#("imul" 0 1
      (:initials
	   "aimul"
	   :summary
	   (concat "asm imul")))
	#("mul" 0 1
      (:initials
	   "amul"
	   :summary
	   (concat "asm mul")))
	#("idiv" 0 1
      (:initials
	   "aidiv"
	   :summary
	   (concat "asm idiv")))
	#("div" 0 1
      (:initials
	   "adiv"
	   :summary
	   (concat "asm div")))
	#("inc" 0 1
      (:initials
	   "ainc"
	   :summary
	   (concat "asm inc")))
	#("dec" 0 1
      (:initials
	   "adec"
	   :summary
	   (concat "asm dec")))
	#("neg" 0 1
      (:initials
	   "aneg"
	   :summary
	   (concat "asm neg")))
	#("daa" 0 1
      (:initials
	   "adaa"
	   :summary
	   (concat "asm daa")))
	#("das" 0 1
      (:initials
	   "adas"
	   :summary
	   (concat "asm das")))
	#("aaa" 0 1
      (:initials
	   "aaaa"
	   :summary
	   (concat "asm aaa")))
	#("aas" 0 1
      (:initials
	   "aaas"
	   :summary
	   (concat "asm aas")))
	#("aam" 0 1
      (:initials
	   "aaam"
	   :summary
	   (concat "asm aam")))
	#("aad" 0 1
      (:initials
	   "aaad"
	   :summary
	   (concat "asm aad")))
	#("inc" 0 1
      (:initials
	   "ainc"
	   :summary
	   (concat "asm inc")))

	;; data compare
	#("cmp" 0 1
      (:initials
	   "acmp"
	   :summary
	   (concat "asm cmp")))
	#("cmpxchg" 0 1
      (:initials
	   "acmpxchg"
	   :summary
	   (concat "asm cmpxchg")))
	#("cmpxchg8b" 0 1
      (:initials
	   "acmpxchg8b"
	   :summary
	   (concat "asm cmpxchg8b")))

	;; data convert
	#("cbw" 0 1
      (:initials
	   "acbw"
	   :summary
	   (concat "asm cbw")))
	#("cwde" 0 1
      (:initials
	   "acwde"
	   :summary
	   (concat "asm cwde")))
	#("cwd" 0 1
      (:initials
	   "acwd"
	   :summary
	   (concat "asm cwd")))
	#("cdq" 0 1
      (:initials
	   "acdq"
	   :summary
	   (concat "asm cdq")))
	#("bswap" 0 1
      (:initials
	   "abswap"
	   :summary
	   (concat "asm bswap")))
	#("movbe" 0 1
      (:initials
	   "amovbe"
	   :summary
	   (concat "asm movbe")))
	#("xlatb" 0 1
      (:initials
	   "axlatb"
	   :summary
	   (concat "asm xlatb")))

	;; Logical operation
	#("and" 0 1
      (:initials
	   "aand"
	   :summary
	   (concat "asm aand")))
	#("or" 0 1
      (:initials
	   "aor"
	   :summary
	   (concat "asm or")))
	#("xor" 0 1
      (:initials
	   "axor"
	   :summary
	   (concat "asm xor")))
	#("not" 0 1
      (:initials
	   "anot"
	   :summary
	   (concat "asm anot")))
	#("test" 0 1
      (:initials
	   "atest"
	   :summary
	   (concat "asm test")))

	;; displacement
	#("rcl" 0 1
      (:initials
	   "arcl"
	   :summary
	   (concat "asm rcl")))
	#("rcr" 0 1
      (:initials
	   "arcr"
	   :summary
	   (concat "asm rcr")))
	#("rol" 0 1
      (:initials
	   "arol"
	   :summary
	   (concat "asm rol")))
	#("ror" 0 1
      (:initials
	   "aror"
	   :summary
	   (concat "asm ror")))
	#("sal" 0 1
      (:initials
	   "asal"
	   :summary
	   (concat "asm sal")))
	#("shl" 0 1
      (:initials
	   "ashl"
	   :summary
	   (concat "asm ashl")))
	#("sar" 0 1
      (:initials
	   "asar"
	   :summary
	   (concat "asm sar")))
	#("shr" 0 1
      (:initials
	   "ashr"
	   :summary
	   (concat "asm shr")))
	#("shld" 0 1
      (:initials
	   "ashld"
	   :summary
	   (concat "asm shld")))
	#("shrd" 0 1
      (:initials
	   "ashrd"
	   :summary
	   (concat "asm shrd")))

	;; bit
	#("setcc" 0 1
      (:initials
	   "asetcc"
	   :summary
	   (concat "asm setcc")))
	#("bt" 0 1
      (:initials
	   "abt"
	   :summary
	   (concat "asm bt")))
	#("bts" 0 1
      (:initials
	   "abts"
	   :summary
	   (concat "asm bts")))
	#("btr" 0 1
      (:initials
	   "abtr"
	   :summary
	   (concat "asm btr")))
	#("btc" 0 1
      (:initials
	   "abtc"
	   :summary
	   (concat "asm btc")))
	#("bsf" 0 1
      (:initials
	   "absf"
	   :summary
	   (concat "asm bsf")))
	#("bsr" 0 1
      (:initials
	   "absr"
	   :summary
	   (concat "asm bsr")))
	#("test" 0 1
      (:initials
	   "atest"
	   :summary
	   (concat "asm test")))

	;; string
	#("cmpsb" 0 1
      (:initials
	   "acmpsb"
	   :summary
	   (concat "asm cmpsb")))
	#("cmpsw" 0 1
      (:initials
	   "acmpsw"
	   :summary
	   (concat "asm cmpsw")))
	#("cmpsd" 0 1
      (:initials
	   "acmpsd"
	   :summary
	   (concat "asm cmpsd")))
	#("lodsb" 0 1
      (:initials
	   "alodsb"
	   :summary
	   (concat "asm lodsb")))
	#("lodsw" 0 1
      (:initials
	   "alodsw"
	   :summary
	   (concat "asm lodsw")))
	#("lodsd" 0 1
      (:initials
	   "alodsd"
	   :summary
	   (concat "asm lodsd")))
	#("movsb" 0 1
      (:initials
	   "amovsb"
	   :summary
	   (concat "asm movsb")))
	#("movsw" 0 1
      (:initials
	   "amovsw"
	   :summary
	   (concat "asm movsw")))
	#("movsd" 0 1
      (:initials
	   "amovsd"
	   :summary
	   (concat "asm movsd")))
	#("scasb" 0 1
      (:initials
	   "ascasb"
	   :summary
	   (concat "asm scasb")))
	#("scasw" 0 1
      (:initials
	   "ascasw"
	   :summary
	   (concat "asm scasw")))
	#("scasd" 0 1
      (:initials
	   "ascasd"
	   :summary
	   (concat "asm scasd")))
	#("stosb" 0 1
      (:initials
	   "astosb"
	   :summary
	   (concat "asm stosb")))
	#("stosw" 0 1
      (:initials
	   "astosw"
	   :summary
	   (concat "asm stosw")))
	#("stosd" 0 1
      (:initials
	   "astosd"
	   :summary
	   (concat "asm stosd")))
	#("rep" 0 1
      (:initials
	   "arep"
	   :summary
	   (concat "asm rep")))
	#("repe" 0 1
      (:initials
	   "arepe"
	   :summary
	   (concat "asm repe")))
	#("repz" 0 1
      (:initials
	   "arepz"
	   :summary
	   (concat "asm repz")))
	#("repne" 0 1
      (:initials
	   "arepne"
	   :summary
	   (concat "asm repne")))
	#("repnz" 0 1
      (:initials
	   "arepnz"
	   :summary
	   (concat "asm repnz")))

	;; flags oprion
	#("clc" 0 1
      (:initials
	   "aclc"
	   :summary
	   (concat "asm clc")))
	#("stc" 0 1
      (:initials
	   "astc"
	   :summary
	   (concat "asm stc")))
	#("cmc" 0 1
      (:initials
	   "acmc"
	   :summary
	   (concat "asm cmc")))
	#("std" 0 1
      (:initials
	   "astd"
	   :summary
	   (concat "asm std")))
	#("cld" 0 1
      (:initials
	   "acld"
	   :summary
	   (concat "asm cld")))
	#("lahf" 0 1
      (:initials
	   "alahf"
	   :summary
	   (concat "asm lahf")))
	#("sahf" 0 1
      (:initials
	   "sahf"
	   :summary
	   (concat "asm sahf")))
	#("pushfd" 0 1
      (:initials
	   "apushfd"
	   :summary
	   (concat "asm pushfd")))
	#("popfd" 0 1
      (:initials
	   "apopfd"
	   :summary
	   (concat "asm popfd")))

	;; control
	#("jmp" 0 1
      (:initials
	   "ajmp"
	   :summary
	   (concat "asm jmp")))
	#("jcc" 0 1
      (:initials
	   "ajcc"
	   :summary
	   (concat "asm jcc")))
	#("call" 0 1
      (:initials
	   "acall"
	   :summary
	   (concat "asm call")))
	#("ret" 0 1
      (:initials
	   "aret"
	   :summary
	   (concat "asm ret")))
	#("enter" 0 1
      (:initials
	   "aenter"
	   :summary
	   (concat "asm enter")))
	#("leave" 0 1
      (:initials
	   "aleave"
	   :summary
	   (concat "asm leave")))
	#("jecxz" 0 1
      (:initials
	   "ajecxz"
	   :summary
	   (concat "asm jecxz")))
	#("loop" 0 1
      (:initials
	   "aloop"
	   :summary
	   (concat "asm loop")))
	#("loope" 0 1
      (:initials
	   "aloope"
	   :summary
	   (concat "asm loope")))
	#("loopz" 0 1
      (:initials
	   "aloopz"
	   :summary
	   (concat "asm loopz")))
	#("loopne" 0 1
      (:initials
	   "aloopne"
	   :summary
	   (concat "asm loopne")))
	#("loopnz" 0 1
      (:initials
	   "aloopnz"
	   :summary
	   (concat "asm loopnz")))

	;; other
	#("bound" 0 1
      (:initials
	   "abound"
	   :summary
	   (concat "asm bound")))
	#("lea" 0 1
      (:initials
	   "alea"
	   :summary
	   (concat "asm lea")))
	#("nop" 0 1
      (:initials
	   "anop"
	   :summary
	   (concat "asm nop")))
	#("cupid" 0 1
      (:initials
	   "acupid"
	   :summary
	   (concat "asm cupid")))

	))

(defun company-asm--annotation (annotation)
  "Format annotation.
ANNOTATION: annotation."
  (format " [%s]" (get-text-property 0 :initials annotation)))

(defun company-asm--meta (meta)
  "Format meta.
META: meta."
  (get-text-property 0 :summary meta))

(defun company-asm--fuzzy-match (prefix candidate)
  "Fuzzy match key word.
PREFIX: preifx.
CANDIDATE: candidate."
  (cl-subsetp (string-to-list prefix)
              (string-to-list candidate)))

(defun company-asm--manual-asm-backend (command &optional arg &rest ignored)
  "Assmbily company backend.
COMMAND: command.
ARG: arg.
IGNORED: ignored."
  (interactive (list 'interactive))

  (case command
    (interactive (company-begin-backend 'company-asm--manual-asm-backend))
    (prefix (and (or (eq major-mode 'asm-mode) (eq major-mode 'nasm-mode))
				 (company-grab-symbol)))
    (candidates
	 (remove-if-not
      (lambda (c) (company-asm--fuzzy-match arg c))
      company-asm--manual-asm-completions))
    (annotation (company-asm--annotation arg))
    (meta (company-asm--meta arg))
    (no-cache 't)))

(provide 'company-asm)
;;; company-asm.el ends here
