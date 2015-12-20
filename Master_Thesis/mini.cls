%------------------------------------------------------------------------------%
% Klasa do skladu prac dyplomowych w jezykach: polskim i angielskim na         %
% wydziale Matematyki i Nauk Informacyjnych Politechniki Warszawskiej.         %
% Autor: Kamen Radew <radewk@student.mini.pw.edu.pl>                           %
%------------------------------------------------------------------------------%
\NeedsTeXFormat{LaTeX2e}[1995/12/01]
\ProvidesClass{mini}[2011/10/15 v0.3 Praca dyplomowa (MiNI PW)]
%------------------------------------------------------------------------------%
\RequirePackage{ifthen}
\newboolean{english}
\setboolean{english}{false}
\newboolean{lady}
\setboolean{lady}{false}
%------------------------------------------------------------------------------%
\DeclareOption{english}{\setboolean{english}{true}}
\DeclareOption{lady}{\setboolean{lady}{true}}
%\DeclareOption{12pt}{\OptionNotUsed}
%\DeclareOption*{\PassOptionsToClass{\CurrentOption}{report}}
\ProcessOptions\relax
\LoadClass[a4paper,11pt,twoside,onecolumn,openright]{report}
%------------------------------------------------------------------------------%
\RequirePackage{graphicx}
\graphicspath{{graphics/}}
%------------------------------------------------------------------------------%
\RequirePackage{geometry}
\geometry{
  inner=25mm,
  outer=25mm,
  bindingoffset=5mm,
  top=20mm,
  bottom=15mm,  % 20mm
  includeheadfoot
}
%------------------------------------------------------------------------------%
\RequirePackage{fancyhdr}
\pagestyle{fancy}
\fancyhf{}
%\fancyhead[RO,LE]{\slshape\rightmark}
%\fancyhead[LO,RE]{\slshape\leftmark}
%\fancyfoot[C]{\thepage}
\fancyhead[RE]{\slshape\leftmark}
\fancyhead[LO]{\slshape\rightmark}
\fancyhead[LE,RO]{\thepage}
%------------------------------------------------------------------------------%
\renewcommand{\chaptermark}[1]{\markboth{\thechapter.\enspace#1}{}}
\renewcommand{\sectionmark}[1]{\markright{\thesection.\enspace#1}}
%------------------------------------------------------------------------------%
\def\cleardoublepage{\clearpage\if@twoside
\ifodd\c@page\else\hbox{}\thispagestyle{empty}\newpage
\if@twocolumn\hbox{}\newpage\fi\fi\fi}
%------------------------------------------------------------------------------%
\renewcommand{\@seccntformat}[1]{\csname the#1\endcsname.\enspace}
\renewcommand{\numberline}[1]{\hb@xt@\@tempdima{#1.\hfil}}
%------------------------------------------------------------------------------%
\RequirePackage[parfill]{parskip}[2001/04/09]
\RequirePackage{setspace}
\frenchspacing
%------------------------------------------------------------------------------%
\clubpenalty=10000
\widowpenalty=10000
\brokenpenalty=10000
%------------------------------------------------------------------------------%
\RequirePackage{amsmath}
\RequirePackage{amsthm}
\RequirePackage{amsfonts}
\RequirePackage{amssymb}
%------------------------------------------------------------------------------%
\RequirePackage[OT4]{polski}
%\bibliographystyle{plplain}
%------------------------------------------------------------------------------%
\RequirePackage[unicode]{hyperref}
\hypersetup{
  bookmarksopen,
  bookmarksopenlevel=1,
  pdfborder={0 0 0},
  pdfdisplaydoctitle,
  pdfpagelayout={SinglePage},
  pdfstartview={FitH}
}
%------------------------------------------------------------------------------%
\def\@schapter#1{
\phantomsection\addcontentsline{toc}{chapter}{#1}
\markboth{#1}{#1}\@makeschapterhead{#1}\@afterheading}
%------------------------------------------------------------------------------%
\renewcommand{\tableofcontents}{
\if@openright\cleardoublepage\else\clearpage\fi\thispagestyle{plain}
\pdfbookmark[0]{\contentsname}{tableofcontents}
\@makeschapterhead{\contentsname}\@afterheading
\@mkboth{\MakeUppercase\contentsname}{\MakeUppercase\contentsname}
\@starttoc{toc}}
%------------------------------------------------------------------------------%
\def\title#1{\gdef\@title{#1}\hypersetup{pdftitle={#1}}}
\def\@title{\@latex@error{No title given}\@ehc}
\def\titleaux#1{\gdef\@titleaux{#1}}
\def\@titleaux{\@latex@error{No polish title given}\@ehc}
\def\author#1{\gdef\@author{#1}\hypersetup{pdfauthor={#1}}}
\def\@author{\@latex@error{No author given}\@ehc}
\def\supervisor#1{\gdef\@supervisor{#1}}
\def\@supervisor{\@latex@error{No supervisor given}\@ehc}
\def\type#1{\gdef\@type{#1}}
\def\@type{\@latex@error{No type given}\@ehc}
\def\discipline#1{\gdef\@discipline{#1}}
\def\@discipline{\@latex@error{No discipline given}\@ehc}
\def\specialty#1{\gdef\@specialty{#1}}
\def\@specialty{\@latex@error{No specialty given}\@ehc}
\def\monthyear#1{\gdef\@monthyear{#1}}
\def\@monthyear{\@latex@error{No month and year given}\@ehc}
\def\date#1{\gdef\@date{#1}}
\def\@date{\@latex@error{No date given}\@ehc}
\def\album#1{\gdef\@album{#1}}
\def\@album{\@latex@error{No album number given}\@ehc}
%------------------------------------------------------------------------------%
\def\@university{Politechnika Warszawska}
\def\@faculty{Wydzia{\l} Matematyki i Nauk Informacyjnych}
\def\@descriptionfirst{Praca dyplomowa \@type ka}
\def\@descriptionsecond{na kierunku \@discipline}
\def\@descriptionthird{Specjalno\'{s}\'{c} \@specialty}
\def\@authorcap{Autor}
\def\@supervisorcap{Promotor}
\def\@location{Warszawa}
\def\@authorsig{podpis autora}
\def\@supervisorsig{podpis promotora}
\def\@titlepagecap{Strona tytu{\l}owa}
\def\@statementcap{O{\'s}wiadczenie}
%------------------------------------------------------------------------------%
\def\@definition{Definicja}
\def\@theorem{Twierdzenie}
\def\@lemma{Lemat}
\def\@proposition{Stwierdzenie}
\def\@corollary{Wniosek}
\def\@remark{Uwaga}
\def\@example{Przyk{\l}ad}
%------------------------------------------------------------------------------%
\ifthenelse{\boolean{english}}{
  \def\@university{Warsaw University of Technology}
  \def\@faculty{Faculty of Mathematics and~Information~Science}
  \def\@descriptionfirst{\@type's Thesis}
  \def\@descriptionsecond{\@discipline}
    \def\@descriptionthird{\@specialty}
  \def\@authorcap{Author}
  \def\@supervisorcap{Supervisor}
  \def\@location{Warsaw}
  \def\@authorsig{Author's signature}
  \def\@supervisorsig{Supervisor's signature}
  \def\@titlepagecap{Title page}
  \def\@statementcap{Statement}
  %-----------------------------------------%
  \def\@definition{Definition}
  \def\@theorem{Theorem}
  \def\@lemma{Lemma}
  \def\@proposition{Proposition}
  \def\@corollary{Corollary}
  \def\@remark{Remark}
  \def\@example{Example}
}{
  \def\bibname{Literatura}
}
%------------------------------------------------------------------------------%
\newtheoremstyle{thm}{3ex}{3ex}{\itshape}{}{\bfseries}{.}{.5em}{#1 #2\thmnote{ (#3)}}
\newtheoremstyle{rem}{3ex}{3ex}{}{}{\bfseries}{.}{.5em}{#1 #2\thmnote{ (#3)}}
\theoremstyle{thm}
\newtheorem{definition}{\@definition}[chapter]
\newtheorem{theorem}{\@theorem}[chapter]
\newtheorem{lemma}[theorem]{\@lemma}
\newtheorem{proposition}[theorem]{\@proposition}
\newtheorem{corollary}[theorem]{\@corollary}
\theoremstyle{rem}
\newtheorem{remark}{\@remark}[chapter]
\newtheorem{example}{\@example}[chapter]
\renewcommand{\qedsymbol}{\ensuremath{\blacksquare}}
%------------------------------------------------------------------------------%
\renewcommand{\maketitle}{
\pdfbookmark[0]{\@titlepagecap}{titlepage}
\begin{titlepage}
\begin{center}
  \begin{minipage}{25mm}
    \includegraphics[scale=0.75]{pw}
  \end{minipage}
  \hfill
  \begin{minipage}{100mm}
    \begin{center}
      \textsc{\Large\@university\\\vspace{1mm}\large\@faculty}
    \end{center}
  \end{minipage}
  \hfill
  \begin{minipage}{25mm}
    \begin{flushright}
      \includegraphics[scale=0.75]{mini}
    \end{flushright}
  \end{minipage}\par
  \vspace{30mm}
  {\large
    \MakeUppercase{\@descriptionfirst}\\
    \MakeUppercase{\@descriptionsecond}\\
    \MakeUppercase{\@descriptionthird}
  \par}
  \vfill
  \begin{spacing}{1.3}
    \textbf{\Large\MakeUppercase{\@title}}
    \ifthenelse{\boolean{english}}{
      \par\vspace{10mm}
      {\Large\MakeUppercase{\@titleaux}}
    }{}
  \end{spacing}
  \vfill
\end{center}
\hspace{.5\textwidth}
\begin{minipage}[t][70mm]{.5\textwidth}
  \textsc{\large\@authorcap:\\\@author}\par
  \vspace{7mm}
  \textsc{\large\@supervisorcap:\\\@supervisor}
\end{minipage}
\begin{center}
  \textsc{\large\@location, \@monthyear}
\end{center}
\vspace{5mm}
\clearpage
\thispagestyle{empty}
\null\vfill
\begin{minipage}{50mm}
  \begin{center}
    ..............................................\\
    \vspace{1mm}
    \@supervisorsig
  \end{center}
\end{minipage}
\hfill
\begin{minipage}{50mm}
  \begin{center}
    ..............................................\\
    \vspace{1mm}
    \@authorsig
  \end{center}
\end{minipage}
\end{titlepage}}
%------------------------------------------------------------------------------%
\newcommand{\makestatement}{
\cleardoublepage
\thispagestyle{empty}
\pdfbookmark[0]{\@statementcap}{statement}
\@author\hfill Warszawa, \@date\\
Nr albumu \@album
\par\vspace{25mm}
\begin{center}
  {\Large O{\'s}wiadczenie}
\end{center}
\par\vspace{15mm}
O{\'s}wiadczam, {\.z}e prac{\k e} \@type k{\k a} pod
tytu{\l}em ,,\@title'', kt{\'o}rej promotorem jest \@supervisor\
wykona{\l}{\ifthenelse{\boolean{lady}}{a}{e}}m
samodzielnie, co po{\'s}wiadczam w{\l}asnor{\k e}cznym podpisem.
\vspace{15mm}
\begin{flushright}
  \begin{minipage}{50mm}
    \begin{center}
      ..............................................\\
      \vspace{1mm}
      \@author
    \end{center}
  \end{minipage}
\end{flushright}}
%------------------------------------------------------------------------------%
\endinput
