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

type

  TTResultLote_GNRE = class;

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

  TRejeicaoGuiaCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TRejeicaoGuiaCollectionItem;
    procedure SetItem(Index: Integer; Value: TRejeicaoGuiaCollectionItem);
  public
    constructor Create(AOwner: TTResultLote_GNRE); reintroduce;
    function Add: TRejeicaoGuiaCollectionItem;
    property Items[Index: Integer]: TRejeicaoGuiaCollectionItem read GetItem write SetItem; default;
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

  TGuiaCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TGuiaCollectionItem;
    procedure SetItem(Index: Integer; Value: TGuiaCollectionItem);
  public
    constructor Create(AOwner: TTResultLote_GNRE); reintroduce;
    function Add: TGuiaCollectionItem;
    property Items[Index: Integer]: TGuiaCollectionItem read GetItem write SetItem; default;
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
    function Ler_Versao_1: boolean;
    function Ler_Versao_2: boolean;
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

implementation

{ TTResultLote_GNRE }

constructor TTResultLote_GNRE.Create;
begin
  FLeitor := TLeitor.Create;

  FresGuia         := TGuiaCollection.Create(Self);
  FresInfoCabec    := TInfoCabec.Create;
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
begin
  Result := False;

  Leitor.Arquivo := StringReplace(Leitor.Arquivo, 'ns1:', '', [rfReplaceAll]);
  Leitor.Grupo   := Leitor.Arquivo;

  if Leitor.rExtrai(1, 'TResultLote_GNRE') <> '' then
  begin
    FAmbiente     := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'ambiente'));
    FnumeroRecibo := Leitor.rCampo(tcStr, 'numeroRecibo');
    Fresultado    := Leitor.rCampo(tcStr, 'resultado');

    if Leitor.rExtrai(2, 'situacaoProcess') <> '' then
    begin
      Fcodigo    := Leitor.rCampo(tcInt, 'codigo');
      Fdescricao := Leitor.rCampo(tcStr, 'descricao');
    end;

    if (Fresultado <> '') and (Pos('versao="2.00"', Fresultado) > 0) then
      Result := Ler_Versao_2
    else
      Result := Ler_Versao_1;
  end;
end;

function TTResultLote_GNRE.Ler_Versao_1: boolean;
var
  SLResultGuia: TStringList;
  i, j, k: Integer;
begin
  SLResultGuia := TStringList.Create;
  Result       := False;

  try
    SLResultGuia.Text := Fresultado;
    j := -1;
    k := -1;

    for i := 0 to SLResultGuia.Count - 1 do
    begin
      if SameText(Copy(SLResultGuia.Strings[i], 1, 1), '0') then
      begin
        FresInfoCabec.FTipoIdentificadoSolicitante := StrToInt(Copy(SLResultGuia.Strings[i], 2, 1));
        FresInfoCabec.FIdentificadorSolicitante    := Trim(Copy(SLResultGuia.Strings[i], 3, 14));
        FresInfoCabec.FNumeroProtocoloLote         := Trim(Copy(SLResultGuia.Strings[i], 17, 10));
        FresInfoCabec.FAmbiente                    := StrToInt(Copy(SLResultGuia.Strings[i], 27, 1));
      end;

      if SameText(Copy(SLResultGuia.Strings[i], 1, 1), '1') then
      begin
        resGuia.Add;
        Inc(j);

        resGuia.Items[j].Identificador          := StrToInt(Copy(SLResultGuia.Strings[i], 1, 1));
        resGuia.Items[j].SequencialGuia         := StrToInt(Copy(SLResultGuia.Strings[i], 2, 4));
        resGuia.Items[j].SituacaoGuia           := Trim(Copy(SLResultGuia.Strings[i], 6, 1));
        resGuia.Items[j].UFFavorecida           := Trim(Copy(SLResultGuia.Strings[i], 7, 2));
        resGuia.Items[j].CodReceita             := StrToInt(Copy(SLResultGuia.Strings[i], 9, 6));
        resGuia.Items[j].TipoDocEmitente        := StrToInt(Copy(SLResultGuia.Strings[i], 15, 1));
        resGuia.Items[j].DocEmitente            := Trim(Copy(SLResultGuia.Strings[i], 16, 16));
        resGuia.Items[j].RazaoSocialEmitente    := Trim(Copy(SLResultGuia.Strings[i], 32, 60));
        resGuia.Items[j].EnderecoEmitente       := Trim(Copy(SLResultGuia.Strings[i], 92, 60));
        resGuia.Items[j].MunicipioEmitente      := Trim(Copy(SLResultGuia.Strings[i], 152, 50));
        resGuia.Items[j].UFEmitente             := Trim(Copy(SLResultGuia.Strings[i], 202, 2));
        resGuia.Items[j].CEPEmitente            := Trim(Copy(SLResultGuia.Strings[i], 204, 8));
        resGuia.Items[j].TelefoneEmitente       := Trim(Copy(SLResultGuia.Strings[i], 212, 11));
        resGuia.Items[j].TipoDocDestinatario    := StrToInt(Copy(SLResultGuia.Strings[i], 223, 1));
        resGuia.Items[j].DocDestinatario        := Trim(Copy(SLResultGuia.Strings[i], 224, 16));
        resGuia.Items[j].MunicipioDestinatario  := Trim(Copy(SLResultGuia.Strings[i], 240, 50));
        resGuia.Items[j].Produto                := Trim(Copy(SLResultGuia.Strings[i], 290, 255));
        resGuia.Items[j].NumDocOrigem           := Copy(SLResultGuia.Strings[i], 545, 18);
        resGuia.Items[j].Convenio               := Trim(Copy(SLResultGuia.Strings[i], 563, 30));
        resGuia.Items[j].InfoComplementares     := Trim(Copy(SLResultGuia.Strings[i], 593, 300));
        resGuia.Items[j].DataVencimento         := Trim(Copy(SLResultGuia.Strings[i], 893, 8));
        resGuia.Items[j].DataLimitePagamento    := Trim(Copy(SLResultGuia.Strings[i], 901, 8));
        resGuia.Items[j].PeriodoReferencia      := Trim(Copy(SLResultGuia.Strings[i], 909, 1));
        resGuia.Items[j].MesAnoReferencia       := Trim(Copy(SLResultGuia.Strings[i], 910, 6));
        resGuia.Items[j].Parcela                := StrToInt(Copy(SLResultGuia.Strings[i], 916, 3));
        resGuia.Items[j].ValorPrincipal         := StrToInt(Copy(SLResultGuia.Strings[i], 919, 15)) / 100;
        resGuia.Items[j].AtualizacaoMonetaria   := StrToInt(Copy(SLResultGuia.Strings[i], 934, 15)) / 100;
        resGuia.Items[j].Juros                  := StrToInt(Copy(SLResultGuia.Strings[i], 949, 15)) / 100;
        resGuia.Items[j].Multa                  := StrToInt(Copy(SLResultGuia.Strings[i], 964, 15)) / 100;
        resGuia.Items[j].RepresentacaoNumerica  := Copy(SLResultGuia.Strings[i], 979, 48);
        resGuia.Items[j].CodigoBarras           := Copy(SLResultGuia.Strings[i], 1027, 44);
        resGuia.Items[j].QtdeVias               := StrToInt(Copy(SLResultGuia.Strings[i], 1071, 1));
        resGuia.Items[j].NumeroControle         := Copy(SLResultGuia.Strings[i], 1072, 16);
        resGuia.Items[j].IdentificadorGuia      := Copy(SLResultGuia.Strings[i], 1088, 10);
        resGuia.Items[j].GuiaGeradaContingencia := StrToInt(Copy(SLResultGuia.Strings[i], 1098, 1));
        resGuia.Items[j].Reservado              := Trim(Copy(SLResultGuia.Strings[i], 1099, 126));
      end;

      if SameText(Copy(SLResultGuia.Strings[i], 1, 1), '2') then
      begin
        resRejeicaGuia.Add;
        Inc(k);

        resRejeicaGuia.Items[k].Identificador      := StrToInt(Copy(SLResultGuia.Strings[i], 1, 1));
        resRejeicaGuia.Items[k].SequencialGuia     := StrToInt(Copy(SLResultGuia.Strings[i], 2, 4));
        resRejeicaGuia.Items[k].NomeCampo          := Copy(SLResultGuia.Strings[i], 6, 30);
        resRejeicaGuia.Items[k].CodMotivoRejeicao  := StrToInt(Copy(SLResultGuia.Strings[i], 36, 3));
        resRejeicaGuia.Items[k].DescMotivoRejeicao := Copy(SLResultGuia.Strings[i], 39, 355);
      end;
    end;

    Result := True;
  except
    SLResultGuia.Free;
  end;
end;

function TTResultLote_GNRE.Ler_Versao_2: boolean;
var
  i, j, k, l: Integer;
begin
  if Leitor.rExtrai(2, 'resultado') <> '' then
  begin
    i := 0;
    while Leitor.rExtrai(3, 'guia', '', i + 1) <> '' do
    begin
      resGuia.Add;

      resGuia.Items[i].SituacaoGuia          := Leitor.rCampo(tcStr, 'situacaoGuia');
      resGuia.Items[i].UFFavorecida          := Leitor.rCampo(tcStr, 'ufFavorecida');
      resGuia.Items[i].tipoGnre              := Leitor.rCampo(tcStr, 'tipoGnre');
      resGuia.Items[i].ValorPrincipal        := Leitor.rCampo(tcDe2, 'valorGNRE');
      resGuia.Items[i].DataLimitePagamento   := Leitor.rCampo(tcStr, 'dataLimitePagamento');
      resGuia.Items[i].IdentificadorGuia     := Leitor.rCampo(tcInt, 'identificadorGuia');
      resGuia.Items[i].NumeroControle        := Leitor.rCampo(tcStr, 'nossoNumero');
      resGuia.Items[i].RepresentacaoNumerica := Leitor.rCampo(tcStr, 'linhaDigitavel');
      resGuia.Items[i].CodigoBarras          := Leitor.rCampo(tcStr, 'codigoBarras');

      if Leitor.rExtrai(4, 'contribuinteEmitente') <> '' then
      begin
        resGuia.Items[i].DocEmitente         := Leitor.rCampo(tcStr, 'CNPJ');
        resGuia.Items[i].RazaoSocialEmitente := Leitor.rCampo(tcStr, 'razaoSocial');
        resGuia.Items[i].EnderecoEmitente    := Leitor.rCampo(tcStr, 'endereco');
        resGuia.Items[i].MunicipioEmitente   := Leitor.rCampo(tcStr, 'municipio');
        resGuia.Items[i].UFEmitente          := Leitor.rCampo(tcStr, 'uf');
        resGuia.Items[i].CEPEmitente         := Leitor.rCampo(tcStr, 'cep');
        resGuia.Items[i].TelefoneEmitente    := Leitor.rCampo(tcStr, 'telefone');

//              Guia.TipoDocEmitente := StrToInt(Copy(SLResultGuia.Strings[i], 15, 1));
      end;

      if Leitor.rExtrai(4, 'itensGNRE') <> '' then
      begin
        j := 0;
        while Leitor.rExtrai(5, 'item', '', j + 1) <> '' do
        begin
          resGuia.Items[i].CodReceita     := Leitor.rCampo(tcInt, 'receita');
          resGuia.Items[i].DataVencimento := Leitor.rCampo(tcStr, 'dataVencimento');

          l := 0;
          while Leitor.rExtrai(6, 'documentoOrigem', '', l + 1) <> '' do
          begin
            if Leitor.rAtributo('tipo=', 'documentoOrigem') = '22' then
              resGuia.Items[i].NumDocOrigem := Leitor.rCampo(tcStr, 'documentoOrigem');

             inc(l);
          end;

          if Leitor.rExtrai(6, 'contribuinteDestinatario') <> '' then
            resGuia.Items[i].DocDestinatario := Leitor.rCampo(tcStr, 'CNPJ');

          if Leitor.rExtrai(6, 'referencia') <> '' then
          begin
            resGuia.Items[i].MesAnoReferencia := Leitor.rCampo(tcStr, 'mes') +
                                                 Leitor.rCampo(tcStr, 'ano');
          end;

          k := 0;
          while Leitor.rExtrai(6, 'valor', '', k + 1) <> '' do
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
              resGuia.Items[i].ValorPrincipal := Leitor.rCampo(tcDe2, 'valor');

            if Leitor.rAtributo('tipo=', 'valor') = '21' then
              resGuia.Items[i].ValorICMS := Leitor.rCampo(tcDe2, 'valor');

            if Leitor.rAtributo('tipo=', 'valor') = '31' then
              resGuia.Items[i].Multa := Leitor.rCampo(tcDe2, 'valor');

            if Leitor.rAtributo('tipo=', 'valor') = '41' then
              resGuia.Items[i].Juros := Leitor.rCampo(tcDe2, 'valor');

            if Leitor.rAtributo('tipo=', 'valor') = '51' then
              resGuia.Items[i].AtualizacaoMonetaria := Leitor.rCampo(tcDe2, 'valor');

            Inc(k);
          end;

          Inc(j);
        end;
      end;

      if Leitor.rExtrai(4, 'informacoesComplementares') <> '' then
        resGuia.Items[i].InfoComplementares := Leitor.rCampo(tcStr, 'informacao');

      Inc(i);
    end;
  end
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
