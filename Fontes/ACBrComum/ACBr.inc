{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/gpl-license.php                           }
{                                                                              }
{ Alexandre Rocha Lima e Marcondes -  arlm@users.sourceforge.net               }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 20/05/2004: Primeira Versao
|*    Alexandre Rocha Lima e Marcondes
|*    Criaçao e Distribuiçao da Primeira Versao
|* 06/04/2005: Atualização
|*    Alexandre Rocha Lima e Marcondes
|*    Suporte para o Kylix 1, 2 e 3; Delphi 8 e 9; identificação da VisualCLX
|* 15/08/2005: Adcionada compatibilidade de suporte a D5 e CLX
|*    Daniel Simoes de Almeida
|*    Se for CLX ou VisualCLX considera DELPHI6_UP com true
|* 21/08/2005: Adcionada compatibilidade com FreePascal e Lazarus
|*    Douglas Fernando Scheibler / Daniel Simoes de Almeida
|* 16/01/2006: Adcionada compatibilidade com o Delphi 2006
|*    Luciano Donato
|* 06/02/2007: Adcionada compatibilidade com o C++ Builder
|*    Thiago Delgado Pinto
|* 23/10/2012: Reformulação do arquivo para utilizar o jedi.inc e kilyx.inc
|*    do projeto Jedi, visando uma melhor utilização do arquivo
|*    Régys Borges da Silveira
******************************************************************************}

// Descomente as linhas abaixo para usar os recursos de Collate e/ou Botão Salvar do Preview do componente Danfe com Rave Reports
// Essas opções funcionam dependendo da sua versão do Rave Reports. Verifique se sua versão suporta o recurso para não causar erro de compilação
//--------------------------------------------------------------------------------
{.$DEFINE RaveCollate}
{.$DEFINE RaveSaveFromPreview}

// Descomente a linha abaixo, se você quer que seu Executável NÃO dependa das DLLs LibXML2, LibXMLSEC,
{.$DEFINE DFE_SEM_OPENSSL}
// Descomente a linha abaixo, se você quer que seu Executável NÃO dependa das DLLs LibXMLSEC, mas continue suportando a LibXML2
{.$DEFINE DFE_SEM_XMLSEC}
// Descomente a linha abaixo, se você não utiliza a obsoleta Capicom para Criptografia (recomendado)
{.$DEFINE DFE_SEM_CAPICOM}
// Descomente a linha abaixo, se você não utiliza a obsoleta MSXML para Assinatura de XML(recomendado)
{.$DEFINE DFE_SEM_MSXML}
// Descomente a linha abaixo, se você não deseja adicionar a Dependência da INDY nos seus Projetos
{.$DEFINE DFE_SEM_INDY}

{$IfDef DFE_SEM_OPENSSL}
  {$Define DFE_SEM_XMLSEC}
{$EndIf}

// Descomente essa linha, para usar a carga de DLL tardia, nas Units do OpenSSL  (Apenas Delphi)
{.$DEFINE USE_DELAYED}

// Descomente essa linha, para usar ZLibExGZ de \ACBr\Fontes\Terceiros\ZLibExGZ
// ZLib de D7 contém bugs e está incompleta. Caso contrário usará compactação nativa, unit "ZLib"
{.$DEFINE USE_ZLibExGZ}

// Para Remover Warnings de CAST causados por WideString/String/AnsiString, descomente a linha abaixo. (Apenas Delphi)
{.$DEFINE REMOVE_CAST_WARN}

// Ajusta FreePascal para o modo Delphi
//------------------------------------------------------------------------------
{$IFDEF FPC}
  {$DEFINE DFE_SEM_INDY}
  {$DEFINE VCL}
  {$DEFINE UNICODE} // Lazarus 0.9.26 usa UnicodeString como default para String
  {$DEFINE DELPHI7}
  {$DEFINE COMPILER7}
  {$DEFINE HAS_FORMATSETTINGS}
  {$DEFINE HAS_REGEXPR}
  {$DEFINE HAS_INLINE}
  {$H+}

  {$UnDef FPC3}
  {$UnDef FPC2}
  {$IFDEF VER2}
   {$DEFINE FPC2}
  {$EndIf}
  {$IFDEF VER3}
   {$DEFINE FPC3}
  {$ENDIF}
  {$IFDEF FPC3}
   {$DEFINE HAS_CHARINSET}
   {$UnDef FPC2}
  {$ENDIF}
{$ELSE}
  {$DEFINE BORLAND}
{$ENDIF}

{$IFDEF REMOVE_CAST_WARN}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN DUPLICATE_CTOR_DTOR OFF}
  {$WARN CVT_ACHAR_TO_WCHAR OFF}
{$ENDIF}


{$IFNDEF MSWINDOWS}   // Linux, apenas OpenSSL, sem Capicom
  {$DEFINE DFE_SEM_CAPICOM}
  {$DEFINE DFE_SEM_MSXML}
  {$DEFINE DFE_SEM_INDY}
  {$UnDef STDCALL}
{$ENDIF}

// utilizar para facilitar a manutenção do ACBr.inc, já que ele era uma cópia
// da jedi, essa mudança permite uma evolução melhor do arquivo.
//------------------------------------------------------------------------------
{$I ACBr_jedi.inc}

{$IFNDEF SUPPORTS_DELAYED_LOADING}
 {$UNDEF USE_DELAYED}
{$ENDIF}

{$IFDEF DELPHI9_UP}
  {$DEFINE HAS_INLINE}
{$ENDIF}
{$IFDEF DELPHI12_UP}
  {$DEFINE HAS_CHARINSET}
{$ENDIF}
{$IFDEF DELPHI15_UP}
  {$DEFINE HAS_FORMATSETTINGS}
  {$DEFINE HAS_REGEXPR}
{$ENDIF}

{$IFNDEF FPC}
 {$DEFINE USE_libeay32}
{$ENDIF}

// Para usar CLX no Windows / Delphi descomente essa linha
//------------------------------------------------------------------------------
//{$DEFINE CLX}

// Para usar VCL no Windows / Delphi descomente essa linha
{$DEFINE VCL}

// Para usar FMX (Firemonkey) Windows / Delphi descomente essa linha
//{$DEFINE FMX}

// Definições multi-plataforma
//------------------------------------------------------------------------------
{$IFDEF WIN64}
  {$DEFINE MSWINDOWS}
{$ENDIF}

{$IFDEF DELPHILANGUAGE}
  {$IFDEF LINUX}
    {$IFNDEF FPC} // Suporte ao FreePascal 
      {$DEFINE VisualCLX}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF CLX}
    {$DEFINE VisualCLX}
  {$ENDIF}

  {$IFDEF CONDITIONALEXPRESSIONS}
    {$IF Declared(QEventType_CMQuit) or Declared(Key_Word_Lower)}
        {$DEFINE VisualCLX}
    {$IFEND}
  {$ENDIF}

  {$IFNDEF VisualCLX}
    {$DEFINE VCL}
  {$ENDIF}

  {$IFDEF VisualCLX}
    {$UNDEF VCL}
    {$DEFINE COMPLIB_CLX} // kept for backward compatibility, use VisualCLX instead 
  {$ENDIF}

  {$IFDEF QUnit}
    {$UNDEF VCL}
    {$DEFINE VisualCLX}
  {$ENDIF}

  {$IFDEF VCL}
    {$DEFINE COMPLIB_VCL} // kept for backward compatibility, use VCL instead 
  {$ENDIF}

  {$IFDEF VisualCLX}
    {$DEFINE DELPHI6_UP} // Inserido para Suporte ao D5 e manter compatibilidade com a CLX 
  {$ENDIF}
{$ENDIF DELPHILANGUAGE}

//------------------------------------------------------------------------------
// Definições automáticas para tipos de licença:
//------------------------------------------------------------------------------
{$IFDEF D5Standard}
 {$DEFINE DelphiPersonalEdition}
{$ENDIF}

{$IFDEF D6PersonalEdition}
 {$DEFINE DelphiPersonalEdition}
{$ENDIF}

{$IFDEF D7PersonalEdition}
 {$DEFINE DelphiPersonalEdition}
{$ENDIF}

// Reduz warnings no D7:
//------------------------------------------------------------------------------
{$IFDEF COMPILER7_UP}
  {$IFNDEF FPC}
    {$WARN UNSAFE_TYPE OFF}
    {$WARN UNSAFE_CODE OFF}
    {$WARN UNSAFE_CAST OFF}
  {$ENDIF}
{$ENDIF}

// Definições para o Quick Report
// Descomente a linha abaixo caso o Quick Report tenha suporte a PDF
//------------------------------------------------------------------------------
{$DEFINE QReport_PDF}

// Definições para o compomente ACBrCTe
// Define o Pacote de Liberação / Descomente o pacote a ser utilizado 
// Atenção: descomente apenas uma das definições
//------------------------------------------------------------------------------
//{$DEFINE PL_103}
//{$DEFINE PL_104}
{$DEFINE PL_200}

// Verificando se a compilação deve sem em modo texto "NOGUI"
// Nota: FPC no Linux sempre liga a diretiva "CONSOLE", por isso criamos a NOGUI
{$IFDEF FRAMEWORK}
  {$DEFINE NOGUI}
{$ENDIF}

{$IFDEF NOGUI}
  {$DEFINE CONSOLE}
{$ENDIF}

// Ative a diretiva abaixo, para usar a Libxml2, libxslt, libxmlsec compilada
// com MinGw ftp://ftp.zlatkovic.com/libxml/64bit/
{.$DEFINE USE_MINGW}
{$IfNDef MSWINDOWS}
  {$DEFINE USE_MINGW}
{$EndIf}

// ZLib em D7 contem muitos bugs e não suporta GZip de forma correta
{$IfNDef FPC}        
 {$IfNDef DELPHIXE2_UP}
  {$Define USE_ZLibExGZ}
 {$EndIf}
{$Else}
 {$UnDef USE_ZLibExGZ} // ZLibExGZ não é compatível com FPC
{$EndIf}

// Compatibilização de Tipos inexistentes em Delphi Linux (POSIX)
{$IFDEF POSIX}
  AnsiString = UTF8String;
  AnsiChar = UTF8Char;
  PAnsiChar = PUTF8Char;
  PPAnsiChar = ^PUTF8Char;
{$ENDIF}

