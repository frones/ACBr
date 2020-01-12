{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  Rafael Teno Dias                               }
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

{******************************************************************************
|* Historico
|*
|* 18/11/2018: Rafael Dias
|*  - Inicio das classes base.
*******************************************************************************}

{$I ACBr.inc}

unit ACBrLibXml2;

interface

uses
  Classes, SysUtils, libxml2;

procedure LibXmlInit();
procedure LibXmlShutDown();

implementation

uses
  TypInfo;

var
  LibXMLLoaded: boolean;

{ ACBrLibXml2 }

procedure LibXmlInit;
begin
  if (LibXMLLoaded) then
    Exit;

  // --Inicializar funções das units do libxml2
  libxml2.Init;

  { Init libxml and libxslt libraries }
  xmlInitCharEncodingHandlers();
  xmlInitGlobals();
  xmlInitThreads();
  xmlInitParser();
  xmlSubstituteEntitiesDefault(1);
  __xmlLoadExtDtdDefaultValue^ := XML_DETECT_IDS or XML_COMPLETE_ATTRS;
  __xmlIndentTreeOutput^ := 1;
  __xmlSaveNoEmptyTags^ := 1;

  LibXMLLoaded := True;
end;

procedure LibXmlShutDown();
begin
  if (not LibXMLLoaded) then
    Exit;

  { Shutdown libxslt/libxml }
  xmlCleanupParser();
  xmlCleanupThreads();  

  LibXMLLoaded := False;
end;

initialization

  LibXMLLoaded := False;

finalization

  LibXmlShutDown;

end.
