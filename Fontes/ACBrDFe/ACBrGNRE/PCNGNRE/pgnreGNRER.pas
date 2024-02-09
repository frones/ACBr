{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliomar Marchetti                              }
{                              Claudemir Vitor Pereira                         }
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

unit pgnreGNRER;

interface

uses
  SysUtils, Classes,
  pcnConversao, pgnreConversao, pcnLeitor, pgnreGNRE;

type

  TGNRER = class(TObject)
  private
    FLeitor: TLeitor;
    FGNRE: TGNRE;
    FVersao: TVersaoGNRE;
  public
    constructor Create(AOwner: TGNRE);
    destructor Destroy; override;

    function LerXml: boolean;
    function LerXml1: boolean;
    function LerXml2: boolean;

    property Leitor: TLeitor     read FLeitor write FLeitor;
    property GNRE: TGNRE         read FGNRE   write FGNRE;
    property Versao: TVersaoGNRE read FVersao write FVersao;
  end;

implementation

uses
  ACBrConsts;

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


function TGNRER.LerXml: boolean;
begin
  if Pos('TDadosGNRE versao="2.00"', FLeitor.Arquivo) > 0 then
    Result := LerXml2
  else
    Result := LerXml1;
end;

function TGNRER.LerXml1: boolean;
var
  i: Integer;
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
    GNRE.c19_municipioEmitente             := Leitor.rCampo(tcStr, 'c19_municipioEmitente');
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
      CampoExtra                   := GNRE.camposExtras.New;
      CampoExtra.CampoExtra.codigo := Leitor.rCampo(tcInt, 'codigo');
      CampoExtra.CampoExtra.tipo   := Leitor.rCampo(tcStr, 'tipo');
      CampoExtra.CampoExtra.valor  := Leitor.rCampo(tcStr, 'valor');
      Inc(i);
    end;
  end;

  Result := true;
end;

function TGNRER.LerXml2: boolean;
var
  i: Integer;
  CampoExtra: TCampoExtraCollectionItem;
  Ok: Boolean;
begin
  if Leitor.rExtrai(1, 'TDadosGNRE') <> '' then
  begin
    GNRE.c01_UfFavorecida      := Leitor.rCampo(tcStr, 'ufFavorecida');
    GNRE.tipoGNRE              := StrToTipoGNRE(Ok, Leitor.rCampo(tcStr, 'tipoGnre'));
    GNRE.c33_dataPagamento     := Leitor.rCampo(tcDat, 'dataPagamento');
    GNRE.c42_identificadorGuia := Leitor.rCampo(tcStr, 'identificadorGuia');
    GNRE.c10_valorTotal        := Leitor.rCampo(tcDe2, 'valorGNRE');

    if Leitor.rExtrai(2, 'contribuinteEmitente') <> '' then
    begin
      GNRE.c16_razaoSocialEmitente := Leitor.rCampo(tcStr, 'razaoSocial');
      GNRE.c18_enderecoEmitente    := Leitor.rCampo(tcStr, 'endereco');
      GNRE.c19_municipioEmitente   := Leitor.rCampo(tcStr, 'municipio');
      GNRE.c20_ufEnderecoEmitente  := Leitor.rCampo(tcStr, 'uf');
      GNRE.c21_cepEmitente         := Leitor.rCampo(tcStr, 'cep');
      GNRE.c22_telefoneEmitente    := Leitor.rCampo(tcStr, 'telefone');

      if Leitor.rExtrai(3, 'identificacao') <> '' then
      begin
        GNRE.c03_idContribuinteEmitente := Leitor.rCampo(tcStr, 'CNPJ');
        GNRE.c27_tipoIdentificacaoEmitente := 1;
        if GNRE.c03_idContribuinteEmitente = '' then
        begin
          GNRE.c03_idContribuinteEmitente    := Leitor.rCampo(tcStr, 'CPF');
          GNRE.c27_tipoIdentificacaoEmitente := 2;
        end;

        GNRE.c17_inscricaoEstadualEmitente := Leitor.rCampo(tcStr, 'IE');
      end;
    end;

    if Leitor.rExtrai(2, 'itensGNRE') <> '' then
    begin
      if Leitor.rExtrai(3, 'item') <> '' then
      begin
        GNRE.c02_receita             := Leitor.rCampo(tcInt, 'receita');
        GNRE.c25_detalhamentoReceita := Leitor.rCampo(tcInt, 'detalhamentoReceita');
        GNRE.c26_produto             := Leitor.rCampo(tcInt, 'produto');
        GNRE.c14_dataVencimento      := Leitor.rCampo(tcDat, 'dataVencimento');
        GNRE.c15_convenio            := Leitor.rCampo(tcStr, 'convenio');

        if Leitor.rExtrai(3, 'documentoOrigem', '', 1) <> '' then
        begin
          GNRE.c28_tipoDocOrigem := Leitor.rAtributo('tipo', 'documentoOrigem');
          GNRE.c04_docOrigem     := Leitor.rCampo(tcStr, 'documentoOrigem');
        end;

        i := 0;
        while Leitor.rExtrai(3, 'valor', '', i + 1) <> '' do
        begin
          {
          11 - Valor Principal ICMS
          12 - Valor Principal Fundo de Pobreza (FP)
          21 - Valor Total ICMS
          22 - Valor Total FP
          31 - Valor Multa ICMS
          32 - Valor Multa FP
          41 - Valor Juros ICMS
          42 - Valor Juros FP
          51 - Valor Atualização Monetaria ICMS
          52 - Valor Atualização Monetaria FP
          }
          if Leitor.rAtributo('tipo=', 'valor') = '11' then
            GNRE.c06_valorPrincipal := Leitor.rCampo(tcDe2, 'valor');

          if Leitor.rAtributo('tipo=', 'valor') = '12' then
            GNRE.ValorFECP := Leitor.rCampo(tcDe2, 'valor');

          if Leitor.rAtributo('tipo=', 'valor') = '21' then
            GNRE.c10_valorTotal := Leitor.rCampo(tcDe2, 'valor');

          if Leitor.rAtributo('tipo=', 'valor') = '22' then
            GNRE.TotalFECP := Leitor.rCampo(tcDe2, 'valor');

          if Leitor.rAtributo('tipo=', 'valor') = '31' then
            GNRE.MultaICMS := Leitor.rCampo(tcDe2, 'valor');

          if Leitor.rAtributo('tipo=', 'valor') = '32' then
            GNRE.MultaFECP := Leitor.rCampo(tcDe2, 'valor');

          if Leitor.rAtributo('tipo=', 'valor') = '41' then
            GNRE.JurosICMS := Leitor.rCampo(tcDe2, 'valor');

          if Leitor.rAtributo('tipo=', 'valor') = '42' then
            GNRE.JurosFECP := Leitor.rCampo(tcDe2, 'valor');

          if Leitor.rAtributo('tipo=', 'valor') = '51' then
            GNRE.AtualMonetICMS := Leitor.rCampo(tcDe2, 'valor');

          if Leitor.rAtributo('tipo=', 'valor') = '52' then
            GNRE.AtualMonetFECP := Leitor.rCampo(tcDe2, 'valor');

          Inc(i);
        end;

        if Leitor.rExtrai(3, 'referencia') <> '' then
        begin
          GNRE.referencia.periodo := Leitor.rCampo(tcInt, 'periodo');
          GNRE.referencia.mes     := Leitor.rCampo(tcStr, 'mes');
          GNRE.referencia.ano     := Leitor.rCampo(tcInt, 'ano');
          GNRE.referencia.parcela := Leitor.rCampo(tcInt, 'parcela');
        end;

        if Leitor.rExtrai(3, 'contribuinteDestinatario') <> '' then
        begin
          GNRE.c37_razaoSocialDestinatario := Leitor.rCampo(tcStr, 'razaoSocial');
          GNRE.c38_municipioDestinatario   := Leitor.rCampo(tcStr, 'municipio');

          if Leitor.rExtrai(4, 'identificacao') <> '' then
          begin
            GNRE.c35_idContribuinteDestinatario    := Leitor.rCampo(tcStr, 'CNPJ');
            GNRE.c34_tipoIdentificacaoDestinatario := 1;

            if GNRE.c35_idContribuinteDestinatario = '' then
            begin
              GNRE.c35_idContribuinteDestinatario    := Leitor.rCampo(tcStr, 'CPF');
              GNRE.c34_tipoIdentificacaoDestinatario := 2;
            end;

            GNRE.c36_inscricaoEstadualDestinatario := Leitor.rCampo(tcStr, 'IE');
          end;
        end;

        i := 0;

        if Leitor.rExtrai(3, 'camposExtras') <> '' then
        begin
          while Leitor.rExtrai(4, 'campoExtra', '', i + 1) <> '' do
          begin
            CampoExtra                   := GNRE.camposExtras.New;
            CampoExtra.CampoExtra.codigo := Leitor.rCampo(tcInt, 'codigo');
            CampoExtra.CampoExtra.valor  := Leitor.rCampo(tcStr, 'valor');
            Inc(i);
          end;
        end;
      end;
    end;
  end;

  Result := true;
end;

end.
