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

unit ACBrGNREGuiasRetorno;

interface

uses
  Classes, 
  Sysutils, 
  ACBrGNREGuiaClass,
  pgnreGNRERetorno, 
  pcnConversao, 
  pcnLeitor;

type

  TGuiaRetorno = class(TCollectionItem)
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

    function GetItem(Index: Integer): TGuiaRetorno;
    procedure SetItem(Index: Integer; const Value: TGuiaRetorno);

    function LerTXT(ArqRetorno: TStringList): Boolean;
    function LerXML(const AXML: String): Boolean;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);

    procedure Imprimir;
    procedure ImprimirPDF;

    function Add: TGuiaRetorno;
    function Insert(Index: Integer): TGuiaRetorno;
    property Items[Index: Integer]: TGuiaRetorno read GetItem  write SetItem;

    function GetNamePath: string; override;
    function LoadFromFile(const CaminhoArquivo: String): boolean;
    function LoadFromString(const Arquivo: String): boolean;

    property ACBrGNRE : TComponent read FACBrGNRE ;
  end;

implementation

uses
  synautil,
  ACBrGNRE2, ACBrUtil.XMLHTML, ACBrUtil.Base,
  ACBrDFeUtil;

{ TGuiaRetorno }

constructor TGuiaRetorno.Create(Collection2: TCollection);
begin
 inherited Create(Collection2);

 FGNRE     := TGNRERetorno.Create;
 FNomeArq  := '';
end;

destructor TGuiaRetorno.Destroy;
begin
  FGNRE.Free;

  inherited Destroy;
end;

procedure TGuiaRetorno.Imprimir;
begin
 if not Assigned( TACBrGNRE( TGuiasRetorno( Collection ).ACBrGNRE ).GNREGuia ) then
   raise Exception.Create('Componente GNREGuia não associado.')
 else
   TACBrGNRE( TGuiasRetorno( Collection ).ACBrGNRE ).GNREGuia.ImprimirGuia(GNRE);
end;

procedure TGuiaRetorno.ImprimirPDF;
begin
 if not Assigned( TACBrGNRE( TGuiasRetorno( Collection ).ACBrGNRE ).GNREGuia ) then
   raise Exception.Create('Componente DANFSE não associado.')
 else
   TACBrGNRE( TGuiasRetorno( Collection ).ACBrGNRE ).GNREGuia.ImprimirGuiaPDF(GNRE);
end;

{ TGuiaRetorno }

function TGuiasRetorno.Add: TGuiaRetorno;
begin
  Result := TGuiaRetorno(inherited Add);
end;

constructor TGuiasRetorno.Create(AOwner: TPersistent;
  ItemClass: TCollectionItemClass);
begin
 if not (AOwner is TACBrGNRE ) then
   raise Exception.Create( 'AOwner deve ser do tipo TGNRE.') ;

 inherited;
 FACBrGNRE := TACBrGNRE( AOwner ) ;
end;

function TGuiasRetorno.GetItem(Index: Integer): TGuiaRetorno;
begin
  Result := TGuiaRetorno(inherited Items[Index]);
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

function TGuiasRetorno.Insert(Index: Integer): TGuiaRetorno;
begin
  Result := TGuiaRetorno(inherited Insert(Index));
end;

function TGuiasRetorno.LerTXT(ArqRetorno: TStringList): Boolean;
var
  GNRERetorno: TGNRERetorno;
  i: Integer;
begin
  Result:= False;
  GNRERetorno := TACBrGNRE(ACBrGNRE).GuiasRetorno.Add.GNRE;

  for i := 0 to ArqRetorno.Count - 1 do
  begin
    if SameText(Copy(ArqRetorno.Strings[i], 1, 1), '0') then
    begin
      GNRERetorno.InfoCabec.TipoIdentificadoSolicitante := StrToInt(Copy(ArqRetorno.Strings[i], 2, 1));
      GNRERetorno.InfoCabec.IdentificadorSolicitante := Trim(Copy(ArqRetorno.Strings[i], 3, 14));
      GNRERetorno.InfoCabec.NumeroProtocoloLote := Trim(Copy(ArqRetorno.Strings[i], 17, 10));
      GNRERetorno.InfoCabec.Ambiente := StrToInt(Copy(ArqRetorno.Strings[i], 27, 1));
      Result := True;
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
      GNRERetorno.MunicipioEmitenteNome := GNRERetorno.MunicipioEmitente;
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
      GNRERetorno.MunicipioDestinatarioNome := GNRERetorno.MunicipioDestinatario;
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
      Result := True;
    end
  end;

end;

function TGuiasRetorno.LerXML(const AXML: String): Boolean;
var
  GNRERetorno: TGNRERetorno;
  i, j, k, Nivel, cProd, codIBGE: Integer;
  xInfo, xUF, xCodUF, tipoValor: string;
  Leitor: TLeitor;
begin
  Result := False;
  Leitor := TLeitor.Create;

  Leitor.Arquivo := StringReplace(AXML, 'ns1:', '', [rfReplaceAll]);
  Leitor.Grupo   := Leitor.Arquivo;

  try
    Nivel := 1;
    i := 0;
    while Leitor.rExtrai(Nivel, 'guia', '', i + 1) <> '' do
    begin
      GNRERetorno := TACBrGNRE(ACBrGNRE).GuiasRetorno.Add.GNRE;
      GNRERetorno.SituacaoGuia          := Leitor.rCampo(tcStr, 'situacaoGuia');
      GNRERetorno.UFFavorecida          := Leitor.rCampo(tcStr, 'ufFavorecida');
      GNRERetorno.tipoGnre              := Leitor.rCampo(tcStr, 'tipoGnre');
      GNRERetorno.valorGNRE             := Leitor.rCampo(tcDe2, 'valorGNRE');
      GNRERetorno.DataLimitePagamento   := DateToStr(Leitor.rCampo(tcDat, 'dataLimitePagamento'));
      GNRERetorno.IdentificadorGuia     := Leitor.rCampo(tcInt, 'identificadorGuia');
      GNRERetorno.NumeroControle        := Leitor.rCampo(tcStr, 'nossoNumero');
      GNRERetorno.RepresentacaoNumerica := Leitor.rCampo(tcStr, 'linhaDigitavel');
      GNRERetorno.CodigoBarras          := Leitor.rCampo(tcStr, 'codigoBarras');
      GNRERetorno.qrcodePayload         := Leitor.rCampo(tcStr, 'qrcodePayload');

      Inc(Nivel);
      if Leitor.rExtrai(Nivel, 'contribuinteEmitente') <> '' then
      begin
        GNRERetorno.DocEmitente         := Leitor.rCampo(tcStr, 'CNPJ');
        if GNRERetorno.DocEmitente = '' then
          GNRERetorno.DocEmitente         := Leitor.rCampo(tcStr, 'CPF');
        if GNRERetorno.DocEmitente = '' then
          GNRERetorno.DocEmitente         := Leitor.rCampo(tcStr, 'IE');

        GNRERetorno.RazaoSocialEmitente := Leitor.rCampo(tcStr, 'razaoSocial');
        GNRERetorno.EnderecoEmitente    := Leitor.rCampo(tcStr, 'endereco');
        GNRERetorno.MunicipioEmitente   := Leitor.rCampo(tcStr, 'municipio');
        GNRERetorno.UFEmitente          := Leitor.rCampo(tcStr, 'uf');

        xCodUF := IntToStr(ObterCodigoUF(GNRERetorno.UFEmitente));
        codIBGE := StrToIntDef(xCodUF + GNRERetorno.MunicipioEmitente, 0);

        if (codIBGE > 0) then
          GNRERetorno.MunicipioEmitenteNome := ObterNomeMunicipio(codIBGE, xUF, '', False);

        if EstaVazio(GNReRetorno.MunicipioEmitenteNome) then
          GNRERetorno.MunicipioEmitenteNome := GNRERetorno.MunicipioEmitente;

        GNRERetorno.CEPEmitente         := Leitor.rCampo(tcStr, 'cep');
        GNRERetorno.TelefoneEmitente    := Leitor.rCampo(tcStr, 'telefone');

        case Length(GNRERetorno.DocEmitente) of
          11: GNRERetorno.TipoDocEmitente := 1;
          14: GNRERetorno.TipoDocEmitente := 2;
        else
          GNRERetorno.TipoDocEmitente := 3;
        end;
      end;

      if Leitor.rExtrai(Nivel, 'informacoesComplementares') <> '' then
      begin
        j := 0;
        while Leitor.rExtrai(Nivel, 'informacao', '', j + 1) <> '' do
        begin
          GNRERetorno.InfoComplementares := GNRERetorno.InfoComplementares + Leitor.rCampo(tcStr, 'informacao')+ sLineBreak;
          Inc(j);
        end;
      end;

      if Leitor.rExtrai(Nivel, 'dadosPagamento') <> '' then
      begin
        GNReRetorno.dadosPagamento.data := Leitor.rCampo(tcDatHor, 'data');
        GNReRetorno.dadosPagamento.autenticacao := Leitor.rCampo(tcStr, 'autenticacao');
        GNReRetorno.dadosPagamento.banco := Leitor.rCampo(tcStr, 'banco');
        GNReRetorno.dadosPagamento.agencia := Leitor.rCampo(tcStr, 'agencia');
        GNReRetorno.dadosPagamento.txId := Leitor.rCampo(tcStr, 'txId');
        GNReRetorno.dadosPagamento.e2eId := Leitor.rCampo(tcStr, 'e2eId');
        GNReRetorno.dadosPagamento.pspPagador := Leitor.rCampo(tcStr, 'pspPagador');
      end;

      if Leitor.rExtrai(Nivel, 'itensGNRE') <> '' then
      begin
        Inc(Nivel);
        j := 0;
        while Leitor.rExtrai(Nivel, 'item', '', j + 1) <> '' do
        begin
          GNRERetorno.CodReceita     := Leitor.rCampo(tcInt, 'receita');
          GNRERetorno.DataVencimento := DateToStr(Leitor.rCampo(tcDat, 'dataVencimento'));
          GNRERetorno.NumDocOrigem   := Leitor.rCampo(tcStr, 'documentoOrigem');
          GNRERetorno.Convenio       := Leitor.rCampo(tcStr, 'convenio');

          // se o tamanho for 44 o conteudo de NumDocOrigem é a chave da NF-e
          // neste caso devemos extrair o numero da nota da chave.
          if Length(GNRERetorno.NumDocOrigem) = 44 then
            GNRERetorno.NumDocOrigem := IntToStr(ExtrairNumeroChaveAcesso(GNRERetorno.NumDocOrigem));

          cProd := StrToIntDef(Leitor.rCampo(tcStr, 'produto'), 0);

          case cProd of
            18: GNRERetorno.Produto := 'Materiais de Construção, Acabamentos, Bricolagens ou Adornos';
            20: GNRERetorno.Produto := 'Peças, Partes, Componentes, Acessórios e demais produtos para Autopropulsados';
            33: GNRERetorno.Produto := 'Comércio Outros não especificados';
            41: GNRERetorno.Produto := 'Ferramentas';
            34: GNRERetorno.Produto := 'Indústria não especificados';
            64: GNRERetorno.Produto := 'Lubrificantes';
            69: GNRERetorno.Produto := 'Motocicletas e ciclomotores';
            84: GNRERetorno.Produto := 'ICMS Complementar Conv. 110/2007';
            89: GNRERetorno.Produto := 'Outros';
            90: GNRERetorno.Produto := 'Peças, Partes e Acessórios para Veículos Automotores';
          else
            GNRERetorno.Produto := IntToStr(cProd);
          end;

//            Inc(Nivel);
          if Leitor.rExtrai(Nivel, 'referencia') <> '' then
          begin
            GNRERetorno.PeriodoReferencia := Leitor.rCampo(tcStr, 'periodo');
            GNRERetorno.MesAnoReferencia := Leitor.rCampo(tcStr, 'mes') +
                                                 Leitor.rCampo(tcStr, 'ano');
          end;

          k := 0;
          while Leitor.rExtrai(Nivel, 'valor', '', k + 1) <> '' do
          begin
            {
            11 - Valor Principal ICMS
            12 - Valor Principal Fundo Estadual de Combate a Pobreza
            21 - Valor Total ICMS
            22 - Valor Total Fundo de Combate a Pobreza
            31 - Valor Multa ICMS
            32 - Valor Multa Fundo de Combate a Pobreza
            41 - Valor Juros ICMS
            42 - Valor Juros Fundo de Combate a Pobreza
            51 - Valor Atualização Monetaria ICMS
            52 - Valor Atualização Monetaria Fundo de Combate a Pobreza
            }
            tipoValor := Leitor.rAtributo('tipo=', 'valor');

            if tipoValor = '11' then
              GNRERetorno.ValorPrincICMS := Leitor.rCampo(tcDe2, 'valor');

            if tipoValor = '12' then
              GNRERetorno.ValorFECP := Leitor.rCampo(tcDe2, 'valor');

            if tipoValor = '21' then
              GNRERetorno.ValorICMS := Leitor.rCampo(tcDe2, 'valor');

            if tipoValor = '22' then
              GNRERetorno.ValorFCP := Leitor.rCampo(tcDe2, 'valor');

            if tipoValor = '31' then
              GNRERetorno.Multa := Leitor.rCampo(tcDe2, 'valor');

            if tipoValor = '32' then
              GNRERetorno.MultaFCP := Leitor.rCampo(tcDe2, 'valor');

            if tipoValor = '41' then
              GNRERetorno.Juros := Leitor.rCampo(tcDe2, 'valor');

            if tipoValor = '42' then
              GNRERetorno.JurosFCP := Leitor.rCampo(tcDe2, 'valor');

            if tipoValor = '51' then
              GNRERetorno.AtualizacaoMonetaria := Leitor.rCampo(tcDe2, 'valor');

            if tipoValor = '52' then
              GNRERetorno.AtualizacaoMonetariaFCP := Leitor.rCampo(tcDe2, 'valor');

            Inc(k);
          end;

          GNRERetorno.ValorPrincipal := GNRERetorno.ValorPrincICMS +
                                        GNRERetorno.ValorFECP;

          if GNRERetorno.ValorPrincipal = 0 then
            GNRERetorno.ValorPrincipal := GNRERetorno.ValorICMS;

          if Leitor.rExtrai(Nivel, 'contribuinteDestinatario') <> '' then
          begin
            GNRERetorno.DocDestinatario       := Leitor.rCampo(tcStr, 'CNPJ');
            GNRERetorno.MunicipioDestinatario := Leitor.rCampo(tcStr, 'municipio');

            xCodUF := IntToStr(ObterCodigoUF(GNRERetorno.UFFavorecida));
            codIBGE := StrToIntDef(xCodUF + GNRERetorno.MunicipioDestinatario, 0);

            if (codIBGE > 0) then
              GNRERetorno.MunicipioDestinatarioNome := ObterNomeMunicipio(codIBGE, xUF, '', False);

            if EstaVazio(GNReRetorno.MunicipioDestinatarioNome) then
              GNRERetorno.MunicipioDestinatarioNome := GNRERetorno.MunicipioDestinatario;

            if GNRERetorno.DocDestinatario = '' then
              GNRERetorno.DocDestinatario       := Leitor.rCampo(tcStr, 'CPF');

            case Length(GNRERetorno.DocDestinatario) of
              11: GNRERetorno.TipoDocDestinatario := 1;
              14: GNRERetorno.TipoDocDestinatario := 2;
            else
              GNRERetorno.TipoDocDestinatario   := 1;
            end;
          end;

          if Leitor.rExtrai(Nivel, 'camposExtras') <> '' then
          begin
            Inc(Nivel);
            k := 0;
            while Leitor.rExtrai(Nivel, 'campoExtra', '', k + 1) <> '' do
            begin
              xInfo := Leitor.rCampo(tcStr, 'valor');

              if Pos(xInfo, GNRERetorno.InfoComplementares) = 0 then
                GNRERetorno.InfoComplementares := GNRERetorno.InfoComplementares +
                                                  LineBreak + xInfo;
              Inc(k);
            end;
          end;

          Inc(j);
        end;
      end;

      Inc(i);
      Nivel := 1;
    end;
  finally
    Leitor.Free;
    Result := True;
  end;
end;

function TGuiasRetorno.LoadFromFile(const CaminhoArquivo: string): boolean;
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
//      Result := False;
      raise;
    end;
  end;
end;

function TGuiasRetorno.LoadFromString(const Arquivo: String): boolean;
var
	ArquivoRetorno: TStringList;
  XMLString: string;
begin
  // Verifica se precisa Converter de UTF8 para a String nativa da IDE //
  XMLString := ConverteXMLtoNativeString(Arquivo);

  if Pos('guia versao="2.00">', XMLString) > 0  then
    Result := LerXML(XMLString)
  else
  begin
    ArquivoRetorno := TStringList.Create;

    try
      ArquivoRetorno.Text := XMLString;
      Result := LerTXT(ArquivoRetorno);

      ArquivoRetorno.Free;
    except
//      Result := False;
      raise;
    end;
  end;
end;

procedure TGuiasRetorno.SetItem(Index: Integer; const Value: TGuiaRetorno);
begin
  Items[Index].Assign(Value);
end;

end.
