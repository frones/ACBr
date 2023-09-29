{$IFNDEF ACBr_INC}
{$DEFINE ACBr_INC}
{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  Alexandre Rocha Lima e Marcondes               }
{                               Regys Silveira                                 }
{                               Juliomar Marchetti                             }
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
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{-----------------------------------------------------
 Esse arquivo é dividido em duas partes: Configurações e Implementação
     * Configurações: Nessa parte estão opções que podem ser ativadas ou desativadas,
          mudando assim o comportamento do código das soluções ACBr. 
             Algumas destas opções são editadas pelo ACBrInstall. Você também 
          pode editar os "DEFINE" dessa parte para atingir o comportamento desejado.
     * Implementação: Essa parte NÃO DEVE SER EDITADA. Fazer isso pode resultar em
          comportamento indefinido e problemas difíceis de diagnosticar.
-----------------------------------------------------}

(*//////////////// CONFIGURAÇÕES  ////////////////////*)

// Descomente a linha abaixo, se você quer que seu Executável NÃO dependa das DLLs LibXML2, LibXMLSEC, (NÃO RECOMENDADO)
{.$DEFINE DFE_SEM_OPENSSL}
// Comente a linha abaixo, se você quer que seu Executável dependa das DLLs LibXMLSEC (NAO RECOMENDADO)
{$DEFINE DFE_SEM_XMLSEC}
// Descomente a linha abaixo, se você quer que seu Executável NÃO dependa das DLLs LibXML2. (NAO RECOMENDADO)
{.$DEFINE DFE_SEM_LIBXML2}
// Descomente a linha abaixo, se você não utiliza a obsoleta Capicom para Criptografia (RECOMENDADO)
{$DEFINE DFE_SEM_CAPICOM}
// Descomente a linha abaixo, se você não utiliza a obsoleta MSXML para Assinatura de XML (RECOMENDADO)
{.$DEFINE DFE_SEM_MSXML}
// Descomente a linha abaixo, se você não deseja adicionar a Dependência da INDY nos seus Projetos
{.$DEFINE DFE_SEM_INDY}

// Descomente essa linha, para usar a carga de DLL tardia, nas Units do OpenSSL  (Apenas Delphi)
{$DEFINE USE_DELAYED}

// Descomente essa linha, para usar ZLibExGZ de \ACBr\Fontes\Terceiros\ZLibExGZ
// ZLib de D7 contém bugs e está incompleta. Caso contrário usará compactação nativa, unit "ZLib"
{.$DEFINE USE_ZLibExGZ}

// Para Remover Warnings de CAST causados por WideString/String/AnsiString, descomente a linha abaixo. (Apenas Delphi)
{.$DEFINE REMOVE_CAST_WARN}

// Para usar CLX no Windows / Delphi descomente essa linha
//------------------------------------------------------------------------------
//{$DEFINE CLX}

// Para usar VCL no Windows / Delphi descomente essa linha
{$DEFINE VCL}

// Para usar FMX (Firemonkey) Windows / Delphi descomente essa linha
//{$DEFINE FMX}

(*//////////////// IMPLEMENTAÇÃO  ////////////////////*)

// Ajusta FreePascal para o modo Delphi
//------------------------------------------------------------------------------
{$IFDEF FPC}
  {$DEFINE USE_UTF8}
  {$UNDEF BORLAND}
  {$DEFINE DFE_SEM_INDY}
  {$DEFINE VCL}
  {$DEFINE UNICODE} // Lazarus 0.9.26 usa UnicodeString como default para String
  {$DEFINE DELPHI7}
  {$DEFINE COMPILER7}
  {$DEFINE HAS_FORMATSETTINGS}
  {$DEFINE HAS_REGEXPR}
  {$DEFINE HAS_INLINE}
  {$DEFINE HAS_STRICTDELIMITER}
  {$H+}

  {$UnDef FMX}
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
   {$DEFINE SUPPORTS_DEPRECATED_DETAILS}
   {$UnDef FPC2}
  {$ENDIF}
  {$UNDEF REMOVE_CAST_WARN} //Devido a problemas com a mensagem de erro "Message recordind not yet supported"
{$ELSE}
  {$DEFINE BORLAND}
{$ENDIF}

// utilizar para facilitar a manutenção do ACBr.inc, já que ele era uma cópia
// da jedi, essa mudança permite uma evolução melhor do arquivo.
//------------------------------------------------------------------------------
{$I ACBr_jedi.inc}

{$IFNDEF DELPHIXE2_UP}
  {$IFDEF BORLAND}
    {$UNDEF REMOVE_CAST_WARN}
  {$ENDIF}
{$ENDIF}

{$IFDEF REMOVE_CAST_WARN}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
  {$WARN SYMBOL_PLATFORM OFF}
  {.$WARN SYMBOL_DEPRECATED OFF}
  {$IFDEF BORLAND}
    {$WARN DUPLICATE_CTOR_DTOR OFF}
    {$WARN CVT_ACHAR_TO_WCHAR OFF}
  {$ENDIF}
{$ENDIF}

{$IFNDEF MSWINDOWS}   // Linux, apenas OpenSSL, sem Capicom
  {$DEFINE DFE_SEM_CAPICOM}
  {$DEFINE DFE_SEM_MSXML}
  {$DEFINE DFE_SEM_INDY}
  {$UnDef STDCALL}
{$ENDIF}

{$IFDEF DFE_SEM_OPENSSL}
  {$DEFINE DFE_SEM_XMLSEC}
  {$DEFINE DFE_SEM_LIBXML2}
{$ENDIF}

{$IFDEF DFE_SEM_LIBXML2}
  {$UNDEF DFE_ACBR_LIBXML2}
{$ENDIF}

{$IFNDEF SUPPORTS_DELAYED_LOADING}
 {$UNDEF USE_DELAYED}
{$ENDIF}

{$IFDEF DELPHI2005_UP}
  {$DEFINE HAS_INLINE}
{$ENDIF}
{$IFDEF DELPHI2006_UP}
  {$DEFINE HAS_STRICTDELIMITER}
{$ENDIF}
{$IFDEF DELPHI2009_UP}
  {$DEFINE HAS_CHARINSET}
  {$DEFINE USE_JSONDATAOBJECTS_UNIT}
{$ENDIF}
{$IFDEF DELPHIXE_UP}
  {$DEFINE HAS_FORMATSETTINGS}
  {$DEFINE HAS_REGEXPR}
{$ENDIF}
{$IfDEF DELPHIXE8_UP}
  {$DEFINE HAS_BLUETOOTH}
  {$DEFINE HAS_SYSTEM_GENERICS}
{$ENDIF}

// Definições multi-plataforma
//------------------------------------------------------------------------------
{$IFDEF WIN64}
  {$DEFINE MSWINDOWS}
  {$DEFINE DFE_SEM_CAPICOM}  // apenas 32 bits
  {$DEFINE DFE_SEM_MSXML}    // apenas 32 bits
{$ENDIF}

{$IFDEF DELPHILANGUAGE}
  {$IFDEF POSIX}
    {$IFDEF LINUX}
      {$DEFINE USE_UTF8}
      {$UNDEF HAS_BLUETOOTH}
    {$ENDIF}
    {$DEFINE FMX}
    {$DEFINE HAS_SYSTEM_GENERICS}

    {$UNDEF CLX}
    {$UNDEF VisualCLX}
    {$UNDEF VCL}
    {$DEFINE DFE_SEM_CAPICOM}
    {$DEFINE DFE_SEM_MSXML}
    {$UNDEF USE_DELAYED}
    {$LEGACYIFEND ON}
  {$ENDIF}

  {$IFDEF NEXTGEN}
    {$DEFINE HAS_SYSTEM_GENERICS}
    {$DEFINE USE_UTF8}
    {$DEFINE HAS_BLUETOOTH}
    {$LEGACYIFEND ON}
    {$ZEROBASEDSTRINGS OFF}  //http://riversoftavg.com/blogs/index.php/2015/01/17/zerobasedstrings-just-dont/
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

  {$IFDEF FMX}
    {$UNDEF VCL}
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

// Verificando se a compilação deve sem em modo texto "NOGUI"
// Nota: FPC no Linux sempre liga a diretiva "CONSOLE", por isso criamos a NOGUI
{$IFDEF FRAMEWORK}
  {$DEFINE NOGUI}
{$ENDIF}

//  Lazarus Android com LAMW, não suporta Dialogs, Forms,
{$IFDEF LAMW}
  {$DEFINE NOGUI}
{$ENDIF}

{$IFDEF NOGUI}
  {$DEFINE CONSOLE}
{$ENDIF}

// Ative a diretiva abaixo, para usar a Libxml2, libxslt, libxmlsec compilada
// com MinGw ftp://ftp.zlatkovic.com/libxml/64bit/
//{$DEFINE USE_MINGW}

// ZLib em D7 contem muitos bugs e não suporta GZip de forma correta
{$IfNDef FPC}        
 {$IfNDef DELPHIXE2_UP}
  {$Define USE_ZLibExGZ}
 {$EndIf}
{$Else}
 {$UnDef USE_ZLibExGZ} // ZLibExGZ não é compatível com FPC
{$EndIf}

// A partir da versão XE2 é preciso usar namespaces,se não definido "Unit scope names"
{$IFNDEF FPC}        
  {$IFDEF DELPHIXE2_UP}
    {$DEFINE USE_NAMESPACES}
  {$ENDIF}
{$ENDIF}


{$ENDIF ~ACBr_INC}
