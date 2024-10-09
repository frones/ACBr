{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

{$I ACBr.inc}

unit ACBrANe.ProviderProprio;

interface

uses
  SysUtils, Classes,
  ACBrXmlDocument,
  ACBrANe.ProviderBase, ACBrANe.WebServicesResponse;

type

  { TACBrANeProviderProprio }

  TACBrANeProviderProprio = class(TACBrANeProvider)
  protected
    procedure Configuracao; override;

    procedure PrepararEnviar(Response: TANeEnviarResponse); override;
    procedure GerarMsgDadosEnviar(Response: TANeEnviarResponse;
      Params: TANeParamsResponse); override;
    procedure TratarRetornoEnviar(Response: TANeEnviarResponse); override;

    procedure PrepararConsultar(Response: TANeConsultarResponse); override;
    procedure GerarMsgDadosConsultar(Response: TANeConsultarResponse;
      Params: TANeParamsResponse); override;
    procedure TratarRetornoConsultar(Response: TANeConsultarResponse); override;

    function AplicarXMLtoUTF8(const AXMLRps: string): string; virtual;
    function AplicarLineBreak(const AXMLRps: string; const ABreak: string): string; virtual;

    procedure ProcessarMensagemErros(RootNode: TACBrXmlNode;
                                     Response: TANeWebserviceResponse;
                                     const AListTag: string = 'ListaMensagemRetorno';
                                     const AMessageTag: string = 'MensagemRetorno'); virtual;

  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.XMLHTML,
  ACBrDFeException,
  ACBrANe, ACBrANeDocumentos, ACBrANe.Consts, ACBrANe.Conversao,
  ACBrANe.WebServicesBase;

{ TACBrANeProviderProprio }

procedure TACBrANeProviderProprio.Configuracao;
begin
  inherited Configuracao;

end;

procedure TACBrANeProviderProprio.PrepararEnviar(Response: TANeEnviarResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrANe(FAOwner).SetStatus(stANeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrANeProviderProprio.GerarMsgDadosEnviar(
  Response: TANeEnviarResponse; Params: TANeParamsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrANeProviderProprio.TratarRetornoEnviar(Response: TANeEnviarResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrANeProviderProprio.PrepararConsultar(Response: TANeConsultarResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrANe(FAOwner).SetStatus(stANeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrANeProviderProprio.GerarMsgDadosConsultar(
  Response: TANeConsultarResponse; Params: TANeParamsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrANeProviderProprio.TratarRetornoConsultar(Response: TANeConsultarResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

function TACBrANeProviderProprio.AplicarXMLtoUTF8(const AXMLRps: string): string;
begin
  Result := ConverteXMLtoUTF8(AXMLRps);
end;

function TACBrANeProviderProprio.AplicarLineBreak(const AXMLRps: string; const ABreak: string): string;
begin
  Result := ChangeLineBreak(AXMLRps, ABreak);
end;

procedure TACBrANeProviderProprio.ProcessarMensagemErros(
  RootNode: TACBrXmlNode; Response: TANeWebserviceResponse;
  const AListTag: string; const AMessageTag: string);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

end.
