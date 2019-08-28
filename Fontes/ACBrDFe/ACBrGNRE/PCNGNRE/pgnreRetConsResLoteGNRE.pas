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

unit pgnreRetConsResLoteGNRE;

interface

uses
  SysUtils, Classes, pcnAuxiliar, pcnConversao, pcnLeitor;
(*
 pgnreConversao;
*)
type
  TTResultLote_GNRE            = class;
  TInfoCabec                   = class;
  TRejeicaoGuiaCollection      = class;
  TRejeicaoGuiaCollectionItem  = class;
  TGuiaCollection              = class;
  TGuiaCollectionItem          = class;

  TTResultLote_GNRE = class(TPersistent)
  private
    FLeitor: TLeitor;
    Fambiente: TpcnTipoAmbiente;
    FnumeroRecibo: string;
    Fcodigo: Integer;
    Fdescricao: string;
    Fresultado: string;
    FresInfoCabec: TInfoCabec;
    FresGuia: TGuiaCollection;
    FresRejeicaoGuia: TRejeicaoGuiaCollection;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: boolean;
  published
    property Leitor: TLeitor read FLeitor write FLeitor;
    property ambiente: TpcnTipoAmbiente read Fambiente write Fambiente;
    property numeroRecibo: string read FnumeroRecibo write FnumeroRecibo;
    property codigo: Integer read Fcodigo write Fcodigo;
    property descricao: string read Fdescricao write Fdescricao;
    property resultado: string read Fresultado write Fresultado;
    property resInfoCabec: TInfoCabec read FresInfoCabec write FresInfoCabec;
    property resGuia: TGuiaCollection read FresGuia write FresGuia;
    property resRejeicaGuia: TRejeicaoGuiaCollection read FresRejeicaoGuia write FresRejeicaoGuia;
  end;

  TInfoCabec = class
  private
    FTipoIdentificadoSolicitante: Integer;
    FIdentificadorSolicitante: string;
    FNumeroProtocoloLote: string;
    FAmbiente: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    property TipoIdentificadoSolicitante: Integer read FTipoIdentificadoSolicitante write FTipoIdentificadoSolicitante;
    property IdentificadorSolicitante: string read FIdentificadorSolicitante write FIdentificadorSolicitante;
    property NumeroProtocoloLote: string read FNumeroProtocoloLote write FNumeroProtocoloLote;
    property Ambiente: Integer read FAmbiente write FAmbiente;
  end;

  TRejeicaoGuiaCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TRejeicaoGuiaCollectionItem;
    procedure SetItem(Index: Integer; Value: TRejeicaoGuiaCollectionItem);
  public
    constructor Create(AOwner: TTResultLote_GNRE); reintroduce;
    function Add: TRejeicaoGuiaCollectionItem;
    property Items[Index: Integer]: TRejeicaoGuiaCollectionItem read GetItem write SetItem; default;
  end;

  TRejeicaoGuiaCollectionItem = class(TCollectionItem)
  private
    FIdentificador: Integer;
    FSequencialGuia: Integer;
    FNomeCampo: string;
    FCodMotivoRejeicao: Integer;
    FDescMotivoRejeicao: string;
  published
    property Identificador: Integer read FIdentificador write FIdentificador;
    property SequencialGuia: Integer read FSequencialGuia write FSequencialGuia;
    property NomeCampo: string read FNomeCampo write FNomeCampo;
    property CodMotivoRejeicao: Integer read FCodMotivoRejeicao write FCodMotivoRejeicao;
    property DescMotivoRejeicao: string read FDescMotivoRejeicao write FDescMotivoRejeicao;
  end;

  TGuiaCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TGuiaCollectionItem;
    procedure SetItem(Index: Integer; Value: TGuiaCollectionItem);
  public
    constructor Create(AOwner: TTResultLote_GNRE); reintroduce;
    function Add: TGuiaCollectionItem;
    property Items[Index: Integer]: TGuiaCollectionItem read GetItem write SetItem; default;
  end;

  TGuiaCollectionItem = class(TCollectionItem)
  private
    FIdentificador: Integer;
    FSequencialGuia: Integer;
    FSituacaoGuia: string;
    FUFFavorecida: string;
    FCodReceita: Integer;
    FTipoDocEmitente: Integer;
    FDocEmitente: string;
    FRazaoSocialEmitente: string;
    FEnderecoEmitente: string;
    FMunicipioEmitente: string;
    FUFEmitente: string;
    FCEPEmitente: string;
    FTelefoneEmitente: string;
    FTipoDocDestinatario: Integer;
    FDocDestinatario: string;
    FMunicipioDestinatario: string;
    FProduto: string;
    FNumDocOrigem: string;
    FConvenio: string;
    FInfoComplementares: string;
    FDataVencimento: string;
    FDataLimitePagamento: string;
    FPeriodoReferencia: string;
    FMesAnoReferencia: string;
    FParcela: Integer;
    FValorPrincipal: Currency;
    FAtualizacaoMonetaria: Currency;
    FJuros: Currency;
    FMulta: Currency;
    FRepresentacaoNumerica: string;
    FCodigoBarras: string;
    FQtdeVias: Integer;
    FNumeroControle: string;
    FIdentificadorGuia: string;
    FGuiaGeradaContingencia: Integer;
    FReservado: string;
    // Versao 2.00
    FtipoGnre: string;
    FValorICMS: Currency;
  published
    property Identificador: Integer read FIdentificador write FIdentificador;
    property SequencialGuia: Integer read FSequencialGuia write FSequencialGuia;
    property SituacaoGuia: string read FSituacaoGuia write FSituacaoGuia;
    property UFFavorecida: string read FUFFavorecida write FUFFavorecida;
    property CodReceita: Integer read FCodReceita write FCodReceita;
    property TipoDocEmitente: Integer read FTipoDocEmitente write FTipoDocEmitente;
    property DocEmitente: string read FDocEmitente write FDocEmitente;
    property RazaoSocialEmitente: string read FRazaoSocialEmitente write FRazaoSocialEmitente;
    property EnderecoEmitente: string read FEnderecoEmitente write FEnderecoEmitente;
    property MunicipioEmitente: string read FMunicipioEmitente write FMunicipioEmitente;
    property UFEmitente: string read FUFEmitente write FUFEmitente;
    property CEPEmitente: string read FCEPEmitente write FCEPEmitente;
    property TelefoneEmitente: string read FTelefoneEmitente write FTelefoneEmitente;
    property TipoDocDestinatario: Integer read FTipoDocDestinatario write FTipoDocDestinatario;
    property DocDestinatario: string read FDocDestinatario write FDocDestinatario;
    property MunicipioDestinatario: string read FMunicipioDestinatario write FMunicipioDestinatario;
    property Produto: string read FProduto write FProduto;
    property NumDocOrigem: string read FNumDocOrigem write FNumDocOrigem;
    property Convenio: string read FConvenio write FConvenio;
    property InfoComplementares: string read FInfoComplementares write FInfoComplementares;
    property DataVencimento: string read FDataVencimento write FDataVencimento;
    property DataLimitePagamento: string read FDataLimitePagamento write FDataLimitePagamento;
    property PeriodoReferencia: string read FPeriodoReferencia write FPeriodoReferencia;
    property MesAnoReferencia: string read FMesAnoReferencia write FMesAnoReferencia;
    property Parcela: Integer read FParcela write FParcela;
    property ValorPrincipal: Currency read FValorPrincipal write FValorPrincipal;
    property AtualizacaoMonetaria: Currency read FAtualizacaoMonetaria write FAtualizacaoMonetaria;
    property Juros: Currency read FJuros write FJuros;
    property Multa: Currency read FMulta write FMulta;
    property RepresentacaoNumerica: string read FRepresentacaoNumerica write FRepresentacaoNumerica;
    property CodigoBarras: string read FCodigoBarras write FCodigoBarras;
    property QtdeVias: Integer read FQtdeVias write FQtdeVias;
    property NumeroControle: string read FNumeroControle write FNumeroControle;
    property IdentificadorGuia: string read FIdentificadorGuia write FIdentificadorGuia;
    property GuiaGeradaContingencia: Integer read FGuiaGeradaContingencia write FGuiaGeradaContingencia;
    property Reservado: string read FReservado write FReservado;
    // Versao 2.00
    property tipoGnre: string read FtipoGnre write FtipoGnre;
    property ValorICMS: Currency read FValorICMS write FValorICMS;
  end;

implementation

{ TTResultLote_GNRE }

constructor TTResultLote_GNRE.Create;
begin
  FLeitor := TLeitor.Create;
  FresGuia := TGuiaCollection.Create(Self);
  FresInfoCabec := TInfoCabec.Create;
  FresRejeicaoGuia := TRejeicaoGuiaCollection.Create(Self);
end;

destructor TTResultLote_GNRE.Destroy;
begin
  FLeitor.Free;
  FresGuia.Free;
  FresInfoCabec.Free;
  FresRejeicaoGuia.Free;
  inherited;
end;

function TTResultLote_GNRE.LerXml: boolean;
var
  ok: Boolean;
  i: Integer;
  SLResultGuia: TStringList;
  Guia: TGuiaCollectionItem;
  GuiaRejeicao: TRejeicaoGuiaCollectionItem;
begin
  Result := False;
  SLResultGuia := TStringList.Create;

  try
    Leitor.Grupo := Leitor.Arquivo;

    if Leitor.rExtrai(1, 'ns1:TResultLote_GNRE') <> '' then
    begin
      FAmbiente     := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'ns1:ambiente'));
      FnumeroRecibo := Leitor.rCampo(tcStr, 'ns1:numeroRecibo');
      Fresultado    := Leitor.rCampo(tcStr, 'ns1:resultado');

      if Leitor.rExtrai(2, 'ns1:situacaoProcess') <> '' then
      begin
        Fcodigo    := Leitor.rCampo(tcInt, 'ns1:codigo');
        Fdescricao := Leitor.rCampo(tcStr, 'ns1:descricao');
      end;

      if Fresultado <> '' then
      begin
        if Pos('ns1:guia versao="2.00"', Fresultado) > 0 then
        begin
          Leitor.Grupo := Fresultado;
          Guia := resGuia.Add;

          Guia.SituacaoGuia := Leitor.rCampo(tcStr, 'ns1:situacaoGuia');
          Guia.UFFavorecida := Leitor.rCampo(tcStr, 'ns1:ufFavorecida');
          Guia.tipoGnre     := Leitor.rCampo(tcStr, 'ns1:tipoGnre');
          Guia.ValorPrincipal := Leitor.rCampo(tcDe2, 'ns1:valorGNRE');
          Guia.DataLimitePagamento := Leitor.rCampo(tcStr, 'ns1:dataLimitePagamento');
          Guia.IdentificadorGuia := Leitor.rCampo(tcInt, 'ns1:identificadorGuia');
          Guia.NumeroControle := Leitor.rCampo(tcStr, 'ns1:nossoNumero');
          Guia.RepresentacaoNumerica := Leitor.rCampo(tcStr, 'ns1:linhaDigitavel');
          Guia.CodigoBarras := Leitor.rCampo(tcStr, 'ns1:codigoBarras');

          if Leitor.rExtrai(2, 'ns1:contribuinteEmitente') <> '' then
          begin
            Guia.DocEmitente := Leitor.rCampo(tcStr, 'ns1:CNPJ');
            Guia.RazaoSocialEmitente := Leitor.rCampo(tcStr, 'ns1:razaoSocial');
            Guia.EnderecoEmitente := Leitor.rCampo(tcStr, 'ns1:endereco');
            Guia.MunicipioEmitente := Leitor.rCampo(tcStr, 'ns1:municipio');
            Guia.UFEmitente := Leitor.rCampo(tcStr, 'ns1:uf');
            Guia.CEPEmitente := Leitor.rCampo(tcStr, 'ns1:cep');
            Guia.TelefoneEmitente := Leitor.rCampo(tcStr, 'ns1:telefone');

//              Guia.TipoDocEmitente := StrToInt(Copy(SLResultGuia.Strings[i], 15, 1));
          end;

          if Leitor.rExtrai(2, 'ns1:itensGNRE') <> '' then
          begin
            if Leitor.rExtrai(3, 'ns1:item') <> '' then
            begin
              Guia.CodReceita := Leitor.rCampo(tcInt, 'ns1:receita');
              Guia.DataVencimento := Leitor.rCampo(tcStr, 'ns1:dataVencimento');

              if Leitor.rExtrai(4, 'ns1:referencia') <> '' then
              begin
                Guia.MesAnoReferencia := Leitor.rCampo(tcStr, 'ns1:mes') +
                                         Leitor.rCampo(tcStr, 'ns1:ano');
              end;

              i := 0;
              while Leitor.rExtrai(4, 'valor', '', i + 1) <> '' do
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
                  Guia.ValorPrincipal := Leitor.rCampo(tcDe2, 'ns1:valor');

                if Leitor.rAtributo('tipo=', 'valor') = '21' then
                  Guia.ValorICMS := Leitor.rCampo(tcDe2, 'ns1:valor');

                if Leitor.rAtributo('tipo=', 'valor') = '31' then
                  Guia.Multa := Leitor.rCampo(tcDe2, 'ns1:valor');

                if Leitor.rAtributo('tipo=', 'valor') = '41' then
                  Guia.Juros := Leitor.rCampo(tcDe2, 'ns1:valor');

                if Leitor.rAtributo('tipo=', 'valor') = '51' then
                  Guia.AtualizacaoMonetaria := Leitor.rCampo(tcDe2, 'ns1:valor');

                inc(i);
              end;
            end;
          end;
        end
        else
        begin
          SLResultGuia.Text := Fresultado;
          for i := 0 to SLResultGuia.Count - 1 do
          begin
            if SameText(Copy(SLResultGuia.Strings[i], 1, 1), '0') then
            begin
              FresInfoCabec.FTipoIdentificadoSolicitante := StrToInt(Copy(SLResultGuia.Strings[i], 2, 1));
              FresInfoCabec.FIdentificadorSolicitante := Trim(Copy(SLResultGuia.Strings[i], 3, 14));
              FresInfoCabec.FNumeroProtocoloLote := Trim(Copy(SLResultGuia.Strings[i], 17, 10));
              FresInfoCabec.FAmbiente := StrToInt(Copy(SLResultGuia.Strings[i], 27, 1));
            end;

            if SameText(Copy(SLResultGuia.Strings[i], 1, 1), '1') then
            begin
              Guia := resGuia.Add;
              Guia.Identificador := StrToInt(Copy(SLResultGuia.Strings[i], 1, 1));
              Guia.SequencialGuia := StrToInt(Copy(SLResultGuia.Strings[i], 2, 4));
              Guia.SituacaoGuia := Trim(Copy(SLResultGuia.Strings[i], 6, 1));
              Guia.UFFavorecida := Trim(Copy(SLResultGuia.Strings[i], 7, 2));
              Guia.CodReceita := StrToInt(Copy(SLResultGuia.Strings[i], 9, 6));
              Guia.TipoDocEmitente := StrToInt(Copy(SLResultGuia.Strings[i], 15, 1));
              Guia.DocEmitente := Trim(Copy(SLResultGuia.Strings[i], 16, 16));
              Guia.RazaoSocialEmitente := Trim(Copy(SLResultGuia.Strings[i], 32, 60));
              Guia.EnderecoEmitente := Trim(Copy(SLResultGuia.Strings[i], 92, 60));
              Guia.MunicipioEmitente := Trim(Copy(SLResultGuia.Strings[i], 152, 50));
              Guia.UFEmitente := Trim(Copy(SLResultGuia.Strings[i], 202, 2));
              Guia.CEPEmitente := Trim(Copy(SLResultGuia.Strings[i], 204, 8));
              Guia.TelefoneEmitente := Trim(Copy(SLResultGuia.Strings[i], 212, 11));
              Guia.TipoDocDestinatario := StrToInt(Copy(SLResultGuia.Strings[i], 223, 1));
              Guia.DocDestinatario := Trim(Copy(SLResultGuia.Strings[i], 224, 16));
              Guia.MunicipioDestinatario := Trim(Copy(SLResultGuia.Strings[i], 240, 50));
              Guia.Produto := Trim(Copy(SLResultGuia.Strings[i], 290, 255));
              Guia.NumDocOrigem := Copy(SLResultGuia.Strings[i], 545, 18);
              Guia.Convenio := Trim(Copy(SLResultGuia.Strings[i], 563, 30));
              Guia.InfoComplementares := Trim(Copy(SLResultGuia.Strings[i], 593, 300));
              Guia.DataVencimento := Trim(Copy(SLResultGuia.Strings[i], 893, 8));
              Guia.DataLimitePagamento := Trim(Copy(SLResultGuia.Strings[i], 901, 8));
              Guia.PeriodoReferencia := Trim(Copy(SLResultGuia.Strings[i], 909, 1));
              Guia.MesAnoReferencia := Trim(Copy(SLResultGuia.Strings[i], 910, 6));
              Guia.Parcela := StrToInt(Copy(SLResultGuia.Strings[i], 916, 3));
              Guia.ValorPrincipal := StrToInt(Copy(SLResultGuia.Strings[i], 919, 15)) / 100;
              Guia.AtualizacaoMonetaria := StrToInt(Copy(SLResultGuia.Strings[i], 934, 15)) / 100;
              Guia.Juros := StrToInt(Copy(SLResultGuia.Strings[i], 949, 15)) / 100;
              Guia.Multa := StrToInt(Copy(SLResultGuia.Strings[i], 964, 15)) / 100;
              Guia.RepresentacaoNumerica := Copy(SLResultGuia.Strings[i], 979, 48);
              Guia.CodigoBarras := Copy(SLResultGuia.Strings[i], 1027, 44);
              Guia.QtdeVias := StrToInt(Copy(SLResultGuia.Strings[i], 1071, 1));
              Guia.NumeroControle := Copy(SLResultGuia.Strings[i], 1072, 16);
              Guia.IdentificadorGuia := Copy(SLResultGuia.Strings[i], 1088, 10);
              Guia.GuiaGeradaContingencia := StrToInt(Copy(SLResultGuia.Strings[i], 1098, 1));
              Guia.Reservado := Trim(Copy(SLResultGuia.Strings[i], 1099, 126));
            end;

            if SameText(Copy(SLResultGuia.Strings[i], 1, 1), '2') then
            begin
              GuiaRejeicao := resRejeicaGuia.Add;
              GuiaRejeicao.Identificador := StrToInt(Copy(SLResultGuia.Strings[i], 1, 1));
              GuiaRejeicao.SequencialGuia := StrToInt(Copy(SLResultGuia.Strings[i], 2, 4));
              GuiaRejeicao.NomeCampo := Copy(SLResultGuia.Strings[i], 6, 30);
              GuiaRejeicao.CodMotivoRejeicao := StrToInt(Copy(SLResultGuia.Strings[i], 36, 3));
              GuiaRejeicao.DescMotivoRejeicao := Copy(SLResultGuia.Strings[i], 39, 355);
            end;
          end;
        end;
      end
      else
      begin
        FresGuia.Add;
        FresRejeicaoGuia.Add;
      end;

      Result := True;
    end;
  except
    Result := false;
  end;
end;

{ TGuiaCollection }

function TGuiaCollection.Add: TGuiaCollectionItem;
begin
  Result := TGuiaCollectionItem(inherited Add);
end;

constructor TGuiaCollection.Create(AOwner: TTResultLote_GNRE);
begin
  inherited Create(TGuiaCollectionItem);
end;

function TGuiaCollection.GetItem(Index: Integer): TGuiaCollectionItem;
begin
  Result := TGuiaCollectionItem(inherited GetItem(Index));
end;

procedure TGuiaCollection.SetItem(Index: Integer;
  Value: TGuiaCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfoCabec }

constructor TInfoCabec.Create;
begin
  FTipoIdentificadoSolicitante := 0;
  FIdentificadorSolicitante := '';
  FNumeroProtocoloLote := '';
  FAmbiente := 0;
end;

destructor TInfoCabec.Destroy;
begin
  inherited Destroy;
end;

{ TRejeicaoGuiaCollection }

function TRejeicaoGuiaCollection.Add: TRejeicaoGuiaCollectionItem;
begin
  Result := TRejeicaoGuiaCollectionItem(inherited Add);
end;

constructor TRejeicaoGuiaCollection.Create(AOwner: TTResultLote_GNRE);
begin
  inherited Create(TRejeicaoGuiaCollectionItem);
end;

function TRejeicaoGuiaCollection.GetItem(
  Index: Integer): TRejeicaoGuiaCollectionItem;
begin
  Result := TRejeicaoGuiaCollectionItem(inherited GetItem(Index));
end;

procedure TRejeicaoGuiaCollection.SetItem(Index: Integer;
  Value: TRejeicaoGuiaCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

end.
