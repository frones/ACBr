{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Leivio Ramos de Fontenele                       }
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

unit pcnGeradorReinf;

interface

uses
  SysUtils, Classes, StrUtils, variants,
  ACBrUtil.Base, ACBrUtil.XMLHTML, ACBrUtil.Strings,
  pcnGerador, pcnLeitor, pcnConversao, pcnAuxiliar,
  ACBrDFeConsts,
  pcnCommonReinf, pcnConversaoReinf;

type
  TGeradorOpcoes = class;
  TReinfEvento = class;

  TReinfEvento = class(TReinf)
  private
    FXMLAssinado: String;
    FXMLOriginal: String;
    FAlertas: String;
    FErroValidacao: String;
    FErroValidacaoCompleto: String;
    FVersaoDF: TVersaoReinf;

    FGerador: TGerador;
    FSchema: TReinfSchema;
    function VersaoReinfToStrHibrido(const t: TVersaoReinf): String;
    procedure SetXML(const Value: AnsiString);
  protected
    {Geradores de Uso Comum}
    FXML: AnsiString;

    procedure GerarCabecalho(const Namespace: String);
    procedure GerarRodape;
    procedure GerarIdeEvento(pEvt: TIdeEvento; const GeraGrupo: boolean = True);
    procedure GerarIdeEvento2(pEvt: TIdeEvento2; const GeraGrupo: boolean = True; GeraRetif: Boolean = True);
    procedure GerarIdeEvento3(pEvt: TIdeEvento3);
    procedure GerarModoAbertura(pModo: TTipoOperacao);
    procedure GerarModoFechamento(pModo: TTipoOperacao);
    procedure GerarIdePeriodo(pIdePeriodo: TidePeriodo; const GroupName: string = 'idePeriodo');
    procedure GerarIdeContri(pEmp: TIdeContri; const GeraGrupo: boolean = True);
  public
    FACBrReinf: TObject; //alimenta no Create

    constructor Create(AACBrReinf: TObject); reintroduce; virtual;//->recebe a instancia da classe TACBrReinf
    destructor Destroy; override;

    function  GerarXML: boolean; virtual; abstract;
    procedure SaveToFile(const CaminhoArquivo: string);
    function  Assinar(const XMLEvento: String; NomeEvento: String): AnsiString;
    function  GerarChaveReinf(const emissao: TDateTime;
                                const CNPJF: string;
                                sequencial: Integer;
                                TpInsc: TtpInsc): String;
    procedure Validar(Schema: TReinfSchema);

    property Alertas: String read FAlertas;
    property ErroValidacao: String read FErroValidacao;
    property ErroValidacaoCompleto: String read FErroValidacaoCompleto;
    property VersaoDF: TVersaoReinf read FVersaoDF write FVersaoDF;

    property Gerador: TGerador  read FGerador write FGerador;
    property schema: TReinfSchema read Fschema write Fschema;
    property XML: AnsiString read FXML write SetXML;
  end;

  TGeradorOpcoes = class(TObject)
  private
    FAjustarTagNro: boolean;
    FGerarTagIPIparaNaoTributado: boolean;
    FGerarTXTSimultaneamente: boolean;
    FNormatizarMunicipios: boolean;
    FGerarTagAssinatura: TpcnTagAssinatura;
    FPathArquivoMunicipios: string;
    FValidarInscricoes: boolean;
    FValidarListaServicos: boolean;
  public
    property AjustarTagNro: boolean read FAjustarTagNro write FAjustarTagNro;
    property GerarTagIPIparaNaoTributado: boolean read FGerarTagIPIparaNaoTributado write FGerarTagIPIparaNaoTributado;
    property GerarTXTSimultaneamente: boolean read FGerarTXTSimultaneamente write FGerarTXTSimultaneamente;
    property NormatizarMunicipios: boolean read FNormatizarMunicipios write FNormatizarMunicipios;
    property GerarTagAssinatura: TpcnTagAssinatura read FGerarTagAssinatura write FGerarTagAssinatura;
    property PathArquivoMunicipios: string read FPathArquivoMunicipios write FPathArquivoMunicipios;
    property ValidarInscricoes: boolean read FValidarInscricoes write FValidarInscricoes;
    property ValidarListaServicos: boolean read FValidarListaServicos write FValidarListaServicos;
  end;

implementation

uses
  ACBrReinf, ACBrDFeSSL, ACBrDFeUtil;

{TReinfEvento}

function TReinfEvento.Assinar(const XMLEvento:string; NomeEvento: String): AnsiString;
var
  XMLAss, ArqXML: string;
begin
  Result := '';

  ArqXML := XMLEvento;

  // XML já deve estar em UTF8, para poder ser assinado //
  ArqXML := ConverteXMLtoUTF8(ArqXML);
  FXMLOriginal := ArqXML;

  with TACBrReinf(FACBrReinf) do
  begin
    if not (SSL.SSLCryptLib in ([cryOpenSSL, cryWinCrypt])) then
      raise EACBrReinfException.Create(ACBRReinf_CErroCryptLib);

    if not (SSL.SSLXmlSignLib in ([xsXmlSec, xsLibXml2])) then
      raise EACBrReinfException.Create(ACBRReinf_CErroSignLib);

    XMLAss := SSL.Assinar(String(ArqXML), 'Reinf', NomeEvento, '', '', '', 'id');
    FXMLAssinado := XMLAss;
    FXMLOriginal := XMLAss;

    XMLAss := StringReplace(XMLAss, '<' + ENCODING_UTF8_STD + '>', '', [rfReplaceAll]);
    XMLAss := StringReplace(XMLAss, '<' + XML_V01 + '>', '', [rfReplaceAll]);

    NomeEvento := NomeEvento + '.xml';

//    if Configuracoes.Arquivos.Salvar then
//      Gravar(NomeEvento, XMLAss, Configuracoes.Arquivos.PathSalvar);

    Result := AnsiString(XMLAss);
    (*
    {$IFDEF DEBUG}
    if Configuracoes.Arquivos.Salvar then
    begin
      With TStringList.Create do
      try
        Text := XMLAss;
        SaveToFile(IncludeTrailingPathDelimiter(Configuracoes.Arquivos.PathSalvar) + NomeEvento);
      finally
        Free;
      end;
    end;
    {$ENDIF}
    *)
  end;
end;

constructor TReinfEvento.Create(AACBrReinf: TObject);
begin
  inherited Create;

  if not(AACBrReinf is TACBrReinf) then
  begin
    raise Exception.Create('Parâmetro AACBrReinf precisa ser do tipo TACBrReinf.');
  end;

  FACBrReinf := AACBrReinf;
  FGerador   := TGerador.Create;

  FGerador.ArquivoFormatoXML     := '';
  FGerador.Opcoes.RetirarEspacos := True;
  FGerador.Opcoes.DecimalChar    := ',';
end;

destructor TReinfEvento.Destroy;
begin
  FGerador.Free;

  inherited;
end;

procedure TReinfEvento.SaveToFile(const CaminhoArquivo: string);
var
  lStr: TStringList;
  lFileName: string;
begin
  lFileName := CaminhoArquivo;
  lStr:= TStringList.Create;
  try
    lStr.Text := string(XML);
    lStr.SaveToFile(ChangeFileExt(lFileName,'.xml'));
  finally
    lStr.Free;
  end;
end;

function TReinfEvento.VersaoReinfToStrHibrido(const t: TVersaoReinf): String;
begin
  with TACBrReinf(FACBrReinf) do
  begin
    Result := VersaoReinfToStr(Configuracoes.Geral.VersaoDF);

    // Versao 1_05_01 migrada para versao 2_01_02
    // Mantidas urls, mudança apenas na URN
    if ( Configuracoes.Geral.VersaoDF = v1_05_01 ) then
      Result := '2_01_02';
  end;
end;

procedure TReinfEvento.SetXML(const Value: AnsiString);
var
  NomeEvento: String;
  Ok: Boolean;
  Leitor: TLeitor;
begin
  FXML := Value;

  if not XmlEstaAssinado(FXML) then
  begin
    NomeEvento := TipoEventoToStrEvento(StringXMLToTipoEvento(Ok, FXML));
    FXML := Assinar(FXML, NomeEvento);
  end;

  if ((not XmlEstaAssinado(FXML)) or (Self.Id = '')) then
  begin
    Leitor := TLeitor.Create;
    try
      Leitor.Grupo := FXML;
      Self.Id := Leitor.rAtributo('id=');
    finally
      Leitor.Free;
    end;
  end;

  if not XmlEstaAssinado(FXML) then
    Validar(TipoEventiToSchemaReinf(StringXMLToTipoEvento(Ok, FXML)));
end;

procedure TReinfEvento.Validar(Schema: TReinfSchema);
var
  Erro, AXML: String;
  EhValido: Boolean;
  Evento: string;
begin
  AXML := FXMLAssinado;
  Evento := SchemaReinfToStr(Schema) + PrefixVersao +
          VersaoReinfToStrHibrido(TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF);

  if EstaVazio(AXML) then
  begin
    Assinar(AXML, Evento);
    AXML := FXMLAssinado;
  end;

  with TACBrReinf(FACBrReinf) do
  begin
    EhValido := SSL.Validar(AXML, Configuracoes.Arquivos.PathSchemas + Evento + '.xsd', Erro);

    if not EhValido then
    begin
      FErroValidacao := ACBrStr('Falha na validação dos dados do evento: ') +
                        Evento + sLineBreak + FAlertas;
      FErroValidacaoCompleto := FErroValidacao + sLineBreak + Erro;

      {$IFDEF DEBUG}
      if Configuracoes.Arquivos.Salvar then
      begin
        with TStringList.Create do
        try
          Add(AXML);
          Add('<!--' + FErroValidacaoCompleto + '-->');
          SaveToFile(Configuracoes.Arquivos.PathSalvar+Evento+'_error' +'.xml');
        finally
          Free;
        end;
      end;
      {$ENDIF}

      raise EACBrReinfException.CreateDef(
        IfThen(Configuracoes.Geral.ExibirErroSchema, ErroValidacaoCompleto,
        ErroValidacao));
    end;
  end;
end;

procedure TReinfEvento.GerarCabecalho(const Namespace: String);
begin
  with TACBrReinf(FACBrReinf) do
  begin
    SSL.NameSpaceURI := ACBRReinf_NAMESPACE_URI + Namespace + '/v' +
                        VersaoReinfToStrHibrido(Configuracoes.Geral.VersaoDF);

    Gerador.wGrupo(ENCODING_UTF8, '', False);
    Gerador.wGrupo('Reinf xmlns="' + SSL.NameSpaceURI+'"');
  end;
end;

function TReinfEvento.GerarChaveReinf(const emissao: TDateTime;
                                const CNPJF: string;
                                sequencial: Integer;
                                TpInsc: TtpInsc): String;
var
  nAno, nMes, nDia, nHora, nMin, nSeg, nMSeg: Word;
begin
  // já foi carregado, não é necessário gerar novamente
  if (Length(Self.Id)=36) then
  begin
    Result := Self.Id;
    Exit;
  end;
  // Se o usuario informar 0; o código numerico sera gerado de maneira aleatória //
  if sequencial = 0 then
   sequencial := Random(99999);

  DecodeDate(emissao, nAno, nMes, nDia);
  DecodeTime(emissao, nHora, nMin, nSeg, nMSeg);
  Result := 'ID';

  Result := Result + TpInscricaoToStr(TpInsc);

  if TACBrReinf(FACBrReinf).Configuracoes.Geral.TipoContribuinte in [tcOrgaoPublico, tcPessoaFisica] then
    Result := Result + copy(OnlyNumber(CNPJF) + '00000000000000', 1, 14)
  else
    Result := Result + copy(OnlyNumber(Copy(CNPJF, 1, 8)) + '00000000000000', 1, 14);

  Result := Result + IntToStrZero(nAno, 4);
  Result := Result + IntToStrZero(nMes, 2);
  Result := Result + IntToStrZero(nDia, 2);
  Result := Result + IntToStrZero(nHora, 2);
  Result := Result + IntToStrZero(nMin, 2);
  Result := Result + IntToStrZero(nSeg, 2);
  Result := Result + IntToStrZero(sequencial, 5);
end;

procedure TReinfEvento.GerarRodape;
begin
  Gerador.wGrupo('/Reinf');
end;

procedure TReinfEvento.GerarIdeEvento(pEvt: TIdeEvento; const GeraGrupo: boolean);
begin
  if GeraGrupo then
    Gerador.wGrupo('ideEvento');

  Gerador.wCampo(tcStr, '', 'tpAmb',   1,  1, 1, TpAmbToStr(TACBrReinf(FACBrReinf).Configuracoes.WebServices.Ambiente));
  Gerador.wCampo(tcStr, '', 'procEmi', 1,  1, 1, ProcEmiReinfToStr(pEvt.ProcEmi));
  Gerador.wCampo(tcStr, '', 'verProc', 1, 20, 1, pEvt.VerProc);

  if GeraGrupo then
  	Gerador.wGrupo('/ideEvento');
end;

procedure TReinfEvento.GerarIdeEvento2(pEvt: TIdeEvento2; const GeraGrupo: boolean = True; GeraRetif: Boolean = True);
begin
  if GeraGrupo then
    Gerador.wGrupo('ideEvento');

  if (GeraRetif) then
    Gerador.wCampo(tcStr, '', 'indRetif', 1, 1, 1, IndRetificacaoToStr(pEvt.indRetif));

  if (IndRetificacaoToStr(pEvt.indRetif) = '2') then
    Gerador.wCampo(tcStr, '', 'nrRecibo', 1, 40, 0, pEvt.nrRecibo);

  Gerador.wCampo(tcStr, '', 'perApur', 7, 7, 1, pEvt.perApur);

  if GeraGrupo then
    GerarIdeEvento(pEvt, False);

  if GeraGrupo then
    Gerador.wGrupo('/ideEvento');
end;

procedure TReinfEvento.GerarIdeEvento3(pEvt: TIdeEvento3);
begin
  Gerador.wGrupo('ideEvento');

  Gerador.wCampo(tcStr, '', 'indRetif', 1, 1, 1, IndRetificacaoToStr(pEvt.indRetif));

  if (IndRetificacaoToStr(pEvt.indRetif) = '2') then
    Gerador.wCampo(tcStr, '', 'nrRecibo', 1, 40, 0, pEvt.nrRecibo);

  Gerador.wCampo(tcDat, '', 'dtApuracao', 10, 10, 1, pEvt.dtApuracao);

  GerarIdeEvento(pEvt, False);

  Gerador.wGrupo('/ideEvento');
end;

procedure TReinfEvento.GerarModoAbertura(pModo: TTipoOperacao);
begin
  case pModo of
    toAlteracao: Gerador.wGrupo('alteracao');
    toExclusao: Gerador.wGrupo('exclusao');
  else
    Gerador.wGrupo('inclusao');
  end;
end;

procedure TReinfEvento.GerarModoFechamento(pModo: TTipoOperacao);
begin
  case pModo of
    toAlteracao: Gerador.wGrupo('/alteracao');
    toExclusao: Gerador.wGrupo('/exclusao');
  else
    Gerador.wGrupo('/inclusao');
  end;
end;

procedure TReinfEvento.GerarIdePeriodo(pIdePeriodo: TidePeriodo;
  const GroupName: string);
begin
  Gerador.wGrupo(GroupName);

  Gerador.wCampo(tcStr, '', 'iniValid', 7, 7, 1, pIdePeriodo.IniValid);
  Gerador.wCampo(tcStr, '', 'fimValid', 7, 7, 0, pIdePeriodo.FimValid);

  Gerador.wGrupo('/' + GroupName);
end;

procedure TReinfEvento.GerarIdeContri(pEmp: TIdeContri; const GeraGrupo: boolean = True);
begin
  if GeraGrupo then
    Gerador.wGrupo('ideContri');

  Gerador.wCampo(tcStr, '', 'tpInsc', 1, 1, 1, tpInscricaoToStr(pEmp.TpInsc));

  if (TACBrReinf(FACBrReinf).Configuracoes.Geral.TipoContribuinte in [tcOrgaoPublico, tcPessoaFisica]) then
    Gerador.wCampo(tcStr, '', 'nrInsc', 14, 14, 1, pEmp.NrInsc)
  else
  begin
    if (pEmp.TpInsc = tiCPF) then
      Gerador.wCampo(tcStr, '', 'nrInsc', 11, 11, 1, pEmp.NrInsc)
    else
      Gerador.wCampo(tcStr, '', 'nrInsc', 8, 8, 1, Copy(pEmp.NrInsc, 1, 8));
  end;

  if pEmp.infoComplContri.NatJur <> '' then
  begin
    Gerador.wGrupo('infoComplContri');
    Gerador.wCampo(tcStr, '', 'natJur', 1, 4, 0, pEmp.infoComplContri.NatJur);
    Gerador.wGrupo('/infoComplContri');
  end;

  if GeraGrupo then
    Gerador.wGrupo('/ideContri');
end;

end.

