{******************************************************************************}
{ Projeto: Componente ACBrGNRE                                                 }
{  Biblioteca multiplataforma de componentes Delphi/Lazarus para emissão da    }
{  Guia Nacional de Recolhimento de Tributos Estaduais                         }
{  http://www.gnre.pe.gov.br/                                                  }
{                                                                              }
{ Direitos Autorais Reservados (c) 2013 Claudemir Vitor Pereira                }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                       Juliomar Marchetti                     }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 09/12/2013 - Claudemir Vitor Pereira
|*  - Doação do componente para o Projeto ACBr
******************************************************************************}

{$I ACBr.inc}

unit pgnreGNRER;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnLeitor, pgnreGNRE;

type

  TGNRER = class(TPersistent)
  private
    FLeitor: TLeitor;
    FGNRE: TGNRE;
  public
    constructor Create(AOwner: TGNRE);
    destructor Destroy; override;
    function LerXml: boolean;
  published
    property Leitor: TLeitor read FLeitor write FLeitor;
    property GNRE: TGNRE read FGNRE write FGNRE;
  end;


implementation

uses
  pcnAuxiliar, pgnreConversao,
  ACBrConsts, ACBrUtil;

{ TGNRER }

constructor TGNRER.Create(AOwner: TGNRE);
begin
  FLeitor := TLeitor.Create;
  FGNRE := AOwner;
end;

destructor TGNRER.Destroy;
begin
  FLeitor.Free;
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////

function TGNRER.LerXml: boolean;
var
  i         : integer;
  CampoExtra: TCampoExtraCollectionItem;
begin
  (* Grupo da TAG <TDadosGNRE> ****************************************************** *)
  if Leitor.rExtrai(1, 'TDadosGNRE') <> '' then
  begin
    GNRE.c01_UfFavorecida              := Leitor.rCampo(tcStr, 'c01_UfFavorecida');
    GNRE.c02_receita                   := Leitor.rCampo(tcInt, 'c02_receita');
    GNRE.c25_detalhamentoReceita       := Leitor.rCampo(tcInt, 'c25_detalhamentoReceita');
    GNRE.c26_produto                   := Leitor.rCampo(tcInt, 'c26_produto');
    GNRE.c27_tipoIdentificacaoEmitente := Leitor.rCampo(tcInt, 'c27_tipoIdentificacaoEmitente');
    GNRE.c03_idContribuinteEmitente    := Leitor.rCampo(tcStr, 'c03_idContribuinteEmitente');
    if Leitor.rExtrai(2, 'c03_idContribuinteEmitente') <> '' then
    begin
      if GNRE.c27_tipoIdentificacaoEmitente = 1 then // CNPJ
        GNRE.c03_idContribuinteEmitente := Leitor.rCampo(tcStr, 'CNPJ')
      else
        GNRE.c03_idContribuinteEmitente := Leitor.rCampo(tcStr, 'CPF');
    end;
    Leitor.rExtrai(1, 'TDadosGNRE');
    GNRE.c28_tipoDocOrigem                 := Leitor.rCampo(tcInt, 'c28_tipoDocOrigem');
    GNRE.c04_docOrigem                     := Leitor.rCampo(tcStr, 'c04_docOrigem');
    GNRE.c06_valorPrincipal                := Leitor.rCampo(tcDe2, 'c06_valorPrincipal');
    GNRE.c10_valorTotal                    := Leitor.rCampo(tcDe2, 'c10_valorTotal');
    GNRE.c14_dataVencimento                := Leitor.rCampo(tcDat, 'c14_dataVencimento');
    GNRE.c15_convenio                      := Leitor.rCampo(tcStr, 'c15_convenio');
    GNRE.c16_razaoSocialEmitente           := Leitor.rCampo(tcStr, 'c16_razaoSocialEmitente');
    GNRE.c17_inscricaoEstadualEmitente     := Leitor.rCampo(tcStr, 'c17_inscricaoEstadualEmitente');
    GNRE.c18_enderecoEmitente              := Leitor.rCampo(tcStr, 'c18_enderecoEmitente');
    GNRE.c19_municipioEmitente             := Leitor.rCampo(tcInt, 'c19_municipioEmitente');
    GNRE.c20_ufEnderecoEmitente            := Leitor.rCampo(tcStr, 'c20_ufEnderecoEmitente');
    GNRE.c21_cepEmitente                   := Leitor.rCampo(tcStr, 'c21_cepEmitente');
    GNRE.c22_telefoneEmitente              := Leitor.rCampo(tcStr, 'c22_telefoneEmitente');
    GNRE.c34_tipoIdentificacaoDestinatario := Leitor.rCampo(tcInt, 'c34_tipoIdentificacaoDestinatario');
    GNRE.c35_idContribuinteDestinatario    := Leitor.rCampo(tcStr, 'c35_idContribuinteDestinatario');

    if Leitor.rExtrai(2, 'c35_idContribuinteDestinatario') <> '' then
    begin
      if GNRE.c34_tipoIdentificacaoDestinatario = 1 then // CNPJ
        GNRE.c35_idContribuinteDestinatario := Leitor.rCampo(tcStr, 'CNPJ')
      else
        GNRE.c35_idContribuinteDestinatario := Leitor.rCampo(tcStr, 'CPF');
    end;
    Leitor.rExtrai(1, 'TDadosGNRE');
    GNRE.c36_inscricaoEstadualDestinatario := Leitor.rCampo(tcStr, 'c36_inscricaoEstadualDestinatario');
    GNRE.c37_razaoSocialDestinatario       := Leitor.rCampo(tcStr, 'c37_razaoSocialDestinatario');
    GNRE.c38_municipioDestinatario         := Leitor.rCampo(tcStr, 'c38_municipioDestinatario');
    GNRE.c33_dataPagamento                 := Leitor.rCampo(tcDat, 'c33_dataPagamento');
    GNRE.c42_identificadorGuia             := Leitor.rCampo(tcStr, 'c42_identificadorGuia');
  end;

  if Leitor.rExtrai(1, 'c05_referencia') <> '' then
  begin
    GNRE.referencia.periodo := Leitor.rCampo(tcInt, 'periodo');
    GNRE.referencia.mes     := Leitor.rCampo(tcStr, 'mes');
    GNRE.referencia.ano     := Leitor.rCampo(tcInt, 'ano');
    GNRE.referencia.parcela := Leitor.rCampo(tcInt, 'parcela');
  end;

  i := 0;

  if Leitor.rExtrai(1, 'c39_camposExtras') <> '' then
  begin
    while Leitor.rExtrai(2, 'campoExtra', '', i + 1) <> '' do
    begin
      CampoExtra                   := GNRE.camposExtras.Add;
      CampoExtra.CampoExtra.codigo := Leitor.rCampo(tcInt, 'codigo');
      CampoExtra.CampoExtra.tipo   := Leitor.rCampo(tcStr, 'tipo');
      CampoExtra.CampoExtra.valor  := Leitor.rCampo(tcStr, 'valor');
      Inc(i);
    end;
  end;

  Result := true;
end;

end.
