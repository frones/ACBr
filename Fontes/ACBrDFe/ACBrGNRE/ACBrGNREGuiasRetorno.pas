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

unit ACBrGNREGuiasRetorno;

interface

uses
  Classes, Sysutils, Dialogs, Forms,
  ACBrGNREGuiaClass,
  pgnreGNRERetorno, pcnConversao, pcnAuxiliar, pcnLeitor;

type

  GuiaRetorno = class(TCollectionItem)
  private
    FGNRE: TGNRERetorno;
    FConfirmada : Boolean;
    FMsg : AnsiString;
    FAlertas: AnsiString;
    FNomeArq: String;
  public
    constructor Create(Collection2: TCollection); override;
    destructor Destroy; override;
    procedure Imprimir;
    procedure ImprimirPDF;
    property GNRE: TGNRERetorno  read FGNRE write FGNRE;
    property Confirmada: Boolean  read FConfirmada write FConfirmada;
    property Msg: AnsiString  read FMsg write FMsg;
    property Alertas: AnsiString read FAlertas write FAlertas;
    property NomeArq: String read FNomeArq write FNomeArq;
  end;

  TGuiasRetorno = class(TOwnedCollection)
  private
    FACBrGNRE : TComponent;

    function GetItem(Index: Integer): GuiaRetorno;
    procedure SetItem(Index: Integer; const Value: GuiaRetorno);

    function LerTXT(ArqRetorno: TStringList): Boolean;
    function LerXML(AXML: String): Boolean;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
    procedure Imprimir;
    procedure ImprimirPDF;
    function Add: GuiaRetorno;
    function Insert(Index: Integer): GuiaRetorno;
    property Items[Index: Integer]: GuiaRetorno read GetItem  write SetItem;
    function GetNamePath: string; override ;
    function LoadFromFile(CaminhoArquivo: String): boolean;
    function LoadFromString(Arquivo: String): boolean;
    property ACBrGNRE : TComponent read FACBrGNRE ;
  end;

implementation

uses
  synautil,
  ACBrGNRE2, ACBrUtil, ACBrDFeUtil;

{ GuiaRetorno }

constructor GuiaRetorno.Create(Collection2: TCollection);
begin
 inherited Create(Collection2);
 FGNRE     := TGNRERetorno.Create;
 FNomeArq  := '';
end;

destructor GuiaRetorno.Destroy;
begin
  FGNRE.Free;
  inherited Destroy;
end;

procedure GuiaRetorno.Imprimir;
begin
 if not Assigned( TACBrGNRE( TGuiasRetorno( Collection ).ACBrGNRE ).GNREGuia ) then
   raise Exception.Create('Componente GNREGuia não associado.')
 else
   TACBrGNRE( TGuiasRetorno( Collection ).ACBrGNRE ).GNREGuia.ImprimirGuia(GNRE);
end;

procedure GuiaRetorno.ImprimirPDF;
begin
 if not Assigned( TACBrGNRE( TGuiasRetorno( Collection ).ACBrGNRE ).GNREGuia ) then
   raise Exception.Create('Componente DANFSE não associado.')
 else
   TACBrGNRE( TGuiasRetorno( Collection ).ACBrGNRE ).GNREGuia.ImprimirGuiaPDF(GNRE);
end;

{ TGuiaRetorno }

function TGuiasRetorno.Add: GuiaRetorno;
begin
  Result := GuiaRetorno(inherited Add);
end;

constructor TGuiasRetorno.Create(AOwner: TPersistent;
  ItemClass: TCollectionItemClass);
begin
 if not (AOwner is TACBrGNRE ) then
   raise Exception.Create( 'AOwner deve ser do tipo TGNRE.') ;

 inherited;
 FACBrGNRE := TACBrGNRE( AOwner ) ;
end;

function TGuiasRetorno.GetItem(Index: Integer): GuiaRetorno;
begin
  Result := GuiaRetorno(inherited Items[Index]);
end;

function TGuiasRetorno.GetNamePath: string;
begin
  Result := 'Guia';
end;

procedure TGuiasRetorno.Imprimir;
begin
 if not Assigned( TACBrGNRE( FACBrGNRE ).GNREGuia ) then
   raise Exception.Create('Componente GNREGuia não associado.')
 else
   TACBrGNRE( FACBrGNRE ).GNREGuia.ImprimirGuia(nil);
end;

procedure TGuiasRetorno.ImprimirPDF;
begin
 if not Assigned( TACBrGNRE( FACBrGNRE ).GNREGuia ) then
   raise Exception.Create('Componente GNREGuia não associado.')
 else
   TACBrGNRE( FACBrGNRE ).GNREGuia.ImprimirGuiaPDF(nil);
end;

function TGuiasRetorno.Insert(Index: Integer): GuiaRetorno;
begin
  Result := GuiaRetorno(inherited Insert(Index));
end;

function TGuiasRetorno.LerTXT(ArqRetorno: TStringList): Boolean;
var
  GNRERetorno: TGNRERetorno;
  i: Integer;
begin
  GNRERetorno := TACBrGNRE(ACBrGNRE).GuiasRetorno.Add.GNRE;

  for i := 0 to ArqRetorno.Count - 1 do
  begin
    if SameText(Copy(ArqRetorno.Strings[i], 1, 1), '0') then
    begin
      GNRERetorno.InfoCabec.TipoIdentificadoSolicitante := StrToInt(Copy(ArqRetorno.Strings[i], 2, 1));
      GNRERetorno.InfoCabec.IdentificadorSolicitante := Trim(Copy(ArqRetorno.Strings[i], 3, 14));
      GNRERetorno.InfoCabec.NumeroProtocoloLote := Trim(Copy(ArqRetorno.Strings[i], 17, 10));
      GNRERetorno.InfoCabec.Ambiente := StrToInt(Copy(ArqRetorno.Strings[i], 27, 1));
    end;

    if SameText(Copy(ArqRetorno.Strings[i], 1, 1), '1') then
    begin
      GNRERetorno.Identificador := StrToInt(Copy(ArqRetorno.Strings[i], 1, 1));
      GNRERetorno.SequencialGuia := StrToInt(Copy(ArqRetorno.Strings[i], 2, 4));
      GNRERetorno.SituacaoGuia := Trim(Copy(ArqRetorno.Strings[i], 6, 1));
      GNRERetorno.UFFavorecida := Trim(Copy(ArqRetorno.Strings[i], 7, 2));
      GNRERetorno.CodReceita := StrToInt(Copy(ArqRetorno.Strings[i], 9, 6));
      GNRERetorno.TipoDocEmitente := StrToInt(Copy(ArqRetorno.Strings[i], 15, 1));

      case GNRERetorno.TipoDocEmitente of
        1: GNRERetorno.DocEmitente := Trim(Copy(ArqRetorno.Strings[i], 21, 11));
        2: GNRERetorno.DocEmitente := Trim(Copy(ArqRetorno.Strings[i], 18, 14));
        3: GNRERetorno.DocEmitente := Trim(Copy(ArqRetorno.Strings[i], 16, 16));
      end;

      GNRERetorno.RazaoSocialEmitente := Trim(Copy(ArqRetorno.Strings[i], 32, 60));
      GNRERetorno.EnderecoEmitente := Trim(Copy(ArqRetorno.Strings[i], 92, 60));
      GNRERetorno.MunicipioEmitente := Trim(Copy(ArqRetorno.Strings[i], 152, 50));
      GNRERetorno.UFEmitente := Trim(Copy(ArqRetorno.Strings[i], 202, 2));
      GNRERetorno.CEPEmitente := Trim(Copy(ArqRetorno.Strings[i], 204, 8));
      GNRERetorno.TelefoneEmitente := Trim(Copy(ArqRetorno.Strings[i], 212, 11));
      GNRERetorno.TipoDocDestinatario := StrToInt(Copy(ArqRetorno.Strings[i], 223, 1));

      case GNRERetorno.TipoDocDestinatario of
        1: GNRERetorno.DocDestinatario := Trim(Copy(ArqRetorno.Strings[i], 229, 11));
        2: GNRERetorno.DocDestinatario := Trim(Copy(ArqRetorno.Strings[i], 226, 14));
        3: GNRERetorno.DocDestinatario := Trim(Copy(ArqRetorno.Strings[i], 224, 16));
      end;

      GNRERetorno.MunicipioDestinatario := Trim(Copy(ArqRetorno.Strings[i], 240, 50));
      GNRERetorno.Produto := Trim(Copy(ArqRetorno.Strings[i], 290, 255));
      GNRERetorno.NumDocOrigem := Copy(ArqRetorno.Strings[i], 545, 18);
      GNRERetorno.Convenio := Trim(Copy(ArqRetorno.Strings[i], 563, 30));
      GNRERetorno.InfoComplementares := Trim(Copy(ArqRetorno.Strings[i], 593, 300));
      GNRERetorno.DataVencimento := Trim(Copy(ArqRetorno.Strings[i], 893, 8));
      GNRERetorno.DataLimitePagamento := Trim(Copy(ArqRetorno.Strings[i], 901, 8));
      GNRERetorno.PeriodoReferencia := Trim(Copy(ArqRetorno.Strings[i], 909, 1));
      GNRERetorno.MesAnoReferencia := Trim(Copy(ArqRetorno.Strings[i], 910, 6));
      GNRERetorno.Parcela := StrToInt(Copy(ArqRetorno.Strings[i], 916, 3));
      GNRERetorno.ValorPrincipal := StrToInt(Copy(ArqRetorno.Strings[i], 919, 15)) / 100;
      GNRERetorno.AtualizacaoMonetaria := StrToInt(Copy(ArqRetorno.Strings[i], 934, 15)) / 100;
      GNRERetorno.Juros := StrToInt(Copy(ArqRetorno.Strings[i], 949, 15)) / 100;
      GNRERetorno.Multa := StrToInt(Copy(ArqRetorno.Strings[i], 964, 15)) / 100;
      GNRERetorno.RepresentacaoNumerica := Copy(ArqRetorno.Strings[i], 979, 48);
      GNRERetorno.CodigoBarras := Copy(ArqRetorno.Strings[i], 1027, 44);
      GNRERetorno.QtdeVias := StrToInt(Copy(ArqRetorno.Strings[i], 1071, 1));
      GNRERetorno.NumeroControle := Copy(ArqRetorno.Strings[i], 1072, 16);
      GNRERetorno.IdentificadorGuia := Copy(ArqRetorno.Strings[i], 1088, 10);
      GNRERetorno.GuiaGeradaContingencia := StrToInt(Copy(ArqRetorno.Strings[i], 1098, 1));
      GNRERetorno.Reservado := Trim(Copy(ArqRetorno.Strings[i], 1099, 126));
    end
  end;

  Result := True;
end;

function TGuiasRetorno.LerXML(AXML: String): Boolean;
var
  GNRERetorno: TGNRERetorno;
  i, j, k: Integer;
  Leitor: TLeitor;
begin
  Result := False;
  Leitor := TLeitor.Create;

  Leitor.Arquivo := StringReplace(AXML, 'ns1:', '', [rfReplaceAll]);
  Leitor.Grupo   := Leitor.Arquivo;

  try
    GNRERetorno := TACBrGNRE(ACBrGNRE).GuiasRetorno.Add.GNRE;

    if Leitor.rExtrai(1, 'resultado') <> '' then
    begin
      i := 0;
      while Leitor.rExtrai(2, 'guia', '', i + 1) <> '' do
      begin
        GNRERetorno.SituacaoGuia          := Leitor.rCampo(tcStr, 'situacaoGuia');
        GNRERetorno.UFFavorecida          := Leitor.rCampo(tcStr, 'ufFavorecida');
        GNRERetorno.tipoGnre              := Leitor.rCampo(tcStr, 'tipoGnre');
        GNRERetorno.ValorPrincipal        := Leitor.rCampo(tcDe2, 'valorGNRE');
        GNRERetorno.DataLimitePagamento   := DateToStr(Leitor.rCampo(tcDat, 'dataLimitePagamento'));
        GNRERetorno.IdentificadorGuia     := Leitor.rCampo(tcInt, 'identificadorGuia');
        GNRERetorno.NumeroControle        := Leitor.rCampo(tcStr, 'nossoNumero');
        GNRERetorno.RepresentacaoNumerica := Leitor.rCampo(tcStr, 'linhaDigitavel');
        GNRERetorno.CodigoBarras          := Leitor.rCampo(tcStr, 'codigoBarras');

        if Leitor.rExtrai(3, 'contribuinteEmitente') <> '' then
        begin
          GNRERetorno.DocEmitente         := Leitor.rCampo(tcStr, 'CNPJ');
          GNRERetorno.RazaoSocialEmitente := Leitor.rCampo(tcStr, 'razaoSocial');
          GNRERetorno.EnderecoEmitente    := Leitor.rCampo(tcStr, 'endereco');
          GNRERetorno.MunicipioEmitente   := Leitor.rCampo(tcStr, 'municipio');
          GNRERetorno.UFEmitente          := Leitor.rCampo(tcStr, 'uf');
          GNRERetorno.CEPEmitente         := Leitor.rCampo(tcStr, 'cep');
          GNRERetorno.TelefoneEmitente    := Leitor.rCampo(tcStr, 'telefone');
        end;

        if Leitor.rExtrai(3, 'itensGNRE') <> '' then
        begin
          j := 0;
          while Leitor.rExtrai(4, 'item', '', j + 1) <> '' do
          begin
            GNRERetorno.CodReceita     := Leitor.rCampo(tcInt, 'receita');
            GNRERetorno.DataVencimento := DateToStr(Leitor.rCampo(tcDat, 'dataVencimento'));

            if Leitor.rAtributo('tipo=', 'documentoOrigem') = '10' then
              GNRERetorno.NumDocOrigem := Leitor.rCampo(tcDe2, 'documentoOrigem');

            if Leitor.rExtrai(5, 'referencia') <> '' then
            begin
              GNRERetorno.PeriodoReferencia := Leitor.rCampo(tcStr, 'periodo');
              GNRERetorno.MesAnoReferencia := Leitor.rCampo(tcStr, 'mes') +
                                                   Leitor.rCampo(tcStr, 'ano');
            end;

            k := 0;
            while Leitor.rExtrai(5, 'valor', '', k + 1) <> '' do
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
                GNRERetorno.ValorPrincipal := Leitor.rCampo(tcDe2, 'valor');

              if Leitor.rAtributo('tipo=', 'valor') = '21' then
                GNRERetorno.ValorICMS := Leitor.rCampo(tcDe2, 'valor');

              if Leitor.rAtributo('tipo=', 'valor') = '31' then
                GNRERetorno.Multa := Leitor.rCampo(tcDe2, 'valor');

              if Leitor.rAtributo('tipo=', 'valor') = '41' then
                GNRERetorno.Juros := Leitor.rCampo(tcDe2, 'valor');

              if Leitor.rAtributo('tipo=', 'valor') = '51' then
                GNRERetorno.AtualizacaoMonetaria := Leitor.rCampo(tcDe2, 'valor');

              Inc(k);
            end;

            if Leitor.rExtrai(5, 'contribuinteDestinatario') <> '' then
            begin
              GNRERetorno.DocDestinatario       := Leitor.rCampo(tcStr, 'CNPJ');
              GNRERetorno.MunicipioDestinatario := Leitor.rCampo(tcStr, 'municipio');

              if GNRERetorno.DocDestinatario = '' then
                GNRERetorno.DocDestinatario := Leitor.rCampo(tcStr, 'CPF');
            end;

            if Leitor.rExtrai(5, 'camposExtras') <> '' then
            begin
              k := 0;
              while Leitor.rExtrai(6, 'campoExtra', '', k + 1) <> '' do
              begin
                GNRERetorno.InfoComplementares := GNRERetorno.InfoComplementares +
                                                  Leitor.rCampo(tcStr, 'valor');
                Inc(k);
              end;
            end;

            Inc(j);
          end;
        end;

        Inc(i);
      end;
    end
  finally
    Leitor.Free;
    Result := True;
  end;
end;

function TGuiasRetorno.LoadFromFile(CaminhoArquivo: string): boolean;
var
  XMLUTF8: AnsiString;
  MS: TMemoryStream;
  XMLString: string;
  ArquivoRetorno: TStringList;
begin
  MS := TMemoryStream.Create;
  try
    MS.LoadFromFile(CaminhoArquivo);
    XMLUTF8 := ReadStrFromStream(MS, MS.Size);
  finally
    MS.Free;
  end;

  XMLString := String(XMLUTF8);
  // Verifica se precisa Converter de UTF8 para a String nativa da IDE //
  XMLString := ConverteXMLtoNativeString(XMLString);

  if Pos('guia versao="2.00"', XMLString) > 0  then
    Result := LerXML(XMLString)
  else
  begin
    ArquivoRetorno := TStringList.Create;

    try
      ArquivoRetorno.Text := XMLString;
      Result := LerTXT(ArquivoRetorno);

      ArquivoRetorno.Free;
    except
      Result := False;
      raise;
    end;
  end;
end;

function TGuiasRetorno.LoadFromString(Arquivo: String): boolean;
var
	ArquivoRetorno: TStringList;
  XMLString: string;
begin
  // Verifica se precisa Converter de UTF8 para a String nativa da IDE //
  XMLString := ConverteXMLtoNativeString(Arquivo);

  if Pos('<ns1:guia versao="2.00">', XMLString) > 0  then
    Result := LerXML(XMLString)
  else
  begin
    ArquivoRetorno := TStringList.Create;

    try
      ArquivoRetorno.Text := XMLString;
      Result := LerTXT(ArquivoRetorno);

      ArquivoRetorno.Free;
    except
      Result := False;
      raise;
    end;
  end;
end;

procedure TGuiasRetorno.SetItem(Index: Integer; const Value: GuiaRetorno);
begin
  Items[Index].Assign(Value);
end;

end.
