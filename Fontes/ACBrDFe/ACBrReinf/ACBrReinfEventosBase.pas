{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                             }

{ Direitos Autorais Reservados (c) 2017 Leivio Ramos de Fontenele              }
{                                                                              }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }


{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Leivio Ramos de Fontenele  -  leivio@yahoo.com.br                            }
{******************************************************************************}
{******************************************************************************
|* Historico
|*
|* 24/10/2017: Renato Rubinho
|*  - Compatibilizado Fonte com Delphi 7
*******************************************************************************}

unit ACBrReinfEventosBase;

interface

uses Classes, Sysutils, pcnGerador, pcnConversaoReinf, ACBrReinfClasses, ACBrDFe, Contnrs, TypInfo;

type

  TReinf = class(TPersistent)
  private
    FId: string;
    FACBrReinf: TACBrDFe;
  protected
    function GerarChaveID(const ACNPJF: string; ASequencial: Integer; AOrgaoPublico: Boolean = False): string; virtual;
  public
    constructor Create(AACBrReinf: TACBrDFe); reintroduce;
  published
    function Id(ASequencial: Integer = 0): string;
  end;

  TEventoReinf = class(TReinf)
  private
    FGerador: TGerador;
    FXMLAssinado: string;
    FXMLOriginal: string;
    FXML: AnsiString;
    FSchema: TReinfSchema;
    FErros: string;
    FTipoOperacao: TTypeOperacao;
    FNovaValidade: TIdePeriodo;
    function GetXML: AnsiString;
  protected
    procedure GerarCabecalho(ANamespace: string);
    procedure GerarRodape;
    procedure SetSchema(ASchema: TReinfSchema);
    procedure GerarIdeEvento(ApEvt: TIdeEvento; const AGeraGrupo: boolean = True);
    procedure GerarIdeContribuinte(AContribuinte: TIdeContribuinte);
    procedure GerarEventoXML; virtual; abstract; {Geração do conteudo do evento}
    procedure GerarModoFechamento(AOperacao: TTypeOperacao);
    procedure GerarModoAbertura(AOperacao: TTypeOperacao);
    procedure GerarIdePeriodo(AIdePeriodo: TidePeriodo; const GroupName: string = 'idePeriodo');
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function  GerarXML(AValidar: Boolean = True; ASequencial: integer = 0): boolean; virtual;
    procedure SaveToFile(const ACaminhoArquivo: string);
    function  Assinar(AXMLEvento, ANomeEvento: AnsiString): AnsiString;
    procedure ValidarEventos(AEvento: string);
    property Erros: string read FErros;
    property Gerador: TGerador  read FGerador write FGerador;
    property Schema: TReinfSchema read FSchema;
    property XML: AnsiString read GetXML write FXML;
    property TipoOperacao: TTypeOperacao read FTipoOperacao write FTipoOperacao;
    property NovaValidade: TIdePeriodo read FNovaValidade write FNovaValidade;
  end;

  { TEventoReinfs }
  TEventoReinfs = class(TObjectList)
  private
    function GetItem(Index: Integer): TEventoReinf;
    procedure SetItem(Index: Integer; const Value: TEventoReinf);
  public
    function New(AACBrReinf: TACBrDFe): TEventoReinf;
    property Items[Index: Integer]: TEventoReinf read GetItem write SetItem;
  end;

  TEventoReinfR = class(TEventoReinf)
  private
    FperApur: string;
    FdtApuracao: TDateTime;
  protected
    property TipoOperacao;
    property NovaValidade;
  public
    property perApur: string read FperApur write FperApur;
    property dtApuracao: TDateTime read FdtApuracao write FdtApuracao;
  end;

  TEventoReinfRet = class(TEventoReinfR)
  private
    FindRetif: TIndRetificacao;
    FnrRecibo: string;
  public
    property indRetif: TIndRetificacao read FindRetif write FindRetif;
    property nrRecibo: string read FnrRecibo write FnrRecibo;
  end;

implementation

uses pcnAuxiliar, ACBrUtil, pcnLeitor, ACBrReinf, ACBrReinfUtils, pcnConversao, ACBrDFeUtil, DateUtils;

procedure TEventoReinf.AfterConstruction;
begin
  inherited;
  FGerador := TGerador.Create;
  FNovaValidade := TidePeriodo.Create;

  FGerador.ArquivoFormatoXML := '';
  FGerador.Opcoes.RetirarEspacos := True;
  FGerador.Opcoes.DecimalChar := ',';
end;

function TEventoReinf.Assinar(AXMLEvento, ANomeEvento: AnsiString): AnsiString;
var
  XMLAss: string;
  ArqXML: string;
begin
  Result := '';

  ArqXML := string(AXMLEvento);

  ArqXML := ConverteXMLtoUTF8(ArqXML);
  FXMLOriginal := ArqXML;

  with TACBrReinf(FACBrReinf) do
  begin
    XMLAss := SSL.Assinar(ArqXML, 'Reinf', String(ANomeEvento), '', '', '', 'id');

    FXMLAssinado := XMLAss;
    FXMLOriginal := XMLAss;

    XMLAss := StringReplace(XMLAss, '<' + ENCODING_UTF8_STD + '>', '', [rfReplaceAll]);
    XMLAss := StringReplace(XMLAss, '<' + XML_V01 + '>', '', [rfReplaceAll]);

    ANomeEvento := ANomeEvento + '-' + FId + '.xml';

    if Configuracoes.Arquivos.Salvar then
       Gravar(string(ANomeEvento), XMLAss,Configuracoes.Arquivos.PathSalvar);

    Result := AnsiString(XMLAss);

    {$IFDEF DEBUG}
      With TStringList.Create do
      try
        Text := XMLAss;
        SaveToFile(IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) + 'Docs\' + ANomeEvento);
      finally
        Free;
      end;
    {$ENDIF}
  end;
end;

procedure TEventoReinf.BeforeDestruction;
begin
  inherited;
  FNovaValidade.Free;
  FGerador.Free
end;

procedure TEventoReinf.GerarCabecalho(ANamespace: string);
var
  NameSpaceURI: string;
begin
  NameSpaceURI := 'http://www.reinf.esocial.gov.br/schemas/' + ANamespace +'/' + TACBrReinf( FACBrReinf ).Versao;
  FACBrReinf.SSL.NameSpaceURI := NameSpaceURI;
  Gerador.wGrupo(ENCODING_UTF8, '', False);
  Gerador.wGrupo('Reinf xmlns="' + NameSpaceURI + '"');
end;

procedure TEventoReinf.GerarIdeContribuinte(AContribuinte: TIdeContribuinte);
begin
  Gerador.wGrupo('ideContri');
    Gerador.wCampo(tcStr, '', 'tpInsc', 0, 0, 1, Ord(AContribuinte.TpInsc));
    if AContribuinte.OrgaoPublico then
      Gerador.wCampo(tcStr, '', 'nrInsc', 0, 0, 1, AContribuinte.NrInsc)
    else
      Gerador.wCampo(tcStr, '', 'nrInsc', 0, 0, 1, Copy(AContribuinte.NrInsc, 1, 8));

  // Controle para registros onde existem eventos dentro de Contrib
  if not ( FSchema in [ rsevtAssocDespRec,           // R-2030 - Recursos Recebidos por Associação Desportiva
                        rsevtAssocDespRep,           // R-2040 - Recursos Repassados para Associação Desportiva
                        rsevtEspDesportivo ] ) then  // R-3010 - Receita de Espetáculo Desportivo
    Gerador.wGrupo('/ideContri');
end;

procedure TEventoReinf.GerarIdeEvento(ApEvt: TIdeEvento; const AGeraGrupo: boolean);
begin
  if AGeraGrupo then
    Gerador.wGrupo('ideEvento');

  if Self.InheritsFrom(TEventoReinfRet) then
  begin
    Gerador.wCampo(tcStr, '', 'indRetif', 0, 0, 1, ord(TEventoReinfRet(Self).indRetif));
    if TEventoReinfRet(Self).nrRecibo <> EmptyStr then
      Gerador.wCampo(tcStr, '', 'nrRecibo', 0, 0, 1, TEventoReinfRet(Self).nrRecibo);
  end;

  if Self.InheritsFrom(TEventoReinfR) then
  begin
    if ( TEventoReinfRet(Self).dtApuracao > 0 ) then
      Gerador.wCampo(tcDat, '', 'dtApuracao', 0, 0, 1, TEventoReinfRet(Self).dtApuracao)
    else
      Gerador.wCampo(tcStr, '', 'perApur', 0, 0, 1, TEventoReinfRet(Self).perApur);
  end;

  Gerador.wCampo(tcInt, '', 'tpAmb', 0, 0, 0, ord(ApEvt.TpAmb));
  Gerador.wCampo(tcInt, '', 'procEmi', 0, 0, 0, Ord(ApEvt.ProcEmi));
  Gerador.wCampo(tcStr, '', 'verProc', 0, 0, 0, ApEvt.VerProc);

  if AGeraGrupo then
  	Gerador.wGrupo('/ideEvento');
end;

procedure TEventoReinf.GerarIdePeriodo(AIdePeriodo: TidePeriodo; const GroupName: string);
begin
  Gerador.wGrupo(GroupName);
    Gerador.wCampo(tcStr, '', 'iniValid', 0, 0, 1, AIdePeriodo.IniValid);
    Gerador.wCampo(tcStr, '', 'fimValid', 0, 0, 0, AIdePeriodo.FimValid);
  Gerador.wGrupo('/'+GroupName);
end;

procedure TEventoReinf.GerarModoAbertura(AOperacao: TTypeOperacao);
begin
  case AOperacao of
    toAlteracao: Gerador.wGrupo('alteracao');
    toExclusao: Gerador.wGrupo('exclusao');
  else
    Gerador.wGrupo('inclusao');
  end;
end;

procedure TEventoReinf.GerarModoFechamento(AOperacao: TTypeOperacao);
begin
  case AOperacao of
    toAlteracao: Gerador.wGrupo('/alteracao');
    toExclusao: Gerador.wGrupo('/exclusao');
  else
    Gerador.wGrupo('/inclusao');
  end;
end;

procedure TEventoReinf.GerarRodape;
begin
  Gerador.wGrupo('/Reinf');
end;

function TEventoReinf.GerarXML(AValidar: Boolean = True; ASequencial: integer = 0): boolean;
var
  SchemaStr: string;
begin
  try
    SchemaStr := GetEnumName(TypeInfo(TReinfSchema), Ord(FSchema));
    SchemaStr := Copy( SchemaStr, 3, Length(SchemaStr) - 2 );

    // Gerar Corpo do evento   evtInfoContribuinte
    GerarCabecalho(TReinfSchemaStr[ord(FSchema)]);
      Gerador.wGrupo(SchemaStr + ' id="'+ Self.Id(ASequencial) +'"');
        GerarIdeEvento(TACBrReinf(FACBrReinf).IdeEvento);
        GerarIdeContribuinte(TACBrReinf(FACBrReinf).ideContri);
      GerarEventoXML;

      // Controle para registros onde existem eventos dentro de Contrib
      if ( FSchema in [ rsevtAssocDespRec,           // R-2030 - Recursos Recebidos por Associação Desportiva
                        rsevtAssocDespRep,           // R-2040 - Recursos Repassados para Associação Desportiva
                        rsevtEspDesportivo ] ) then  // R-3010 - Receita de Espetáculo Desportivo
        Gerador.wGrupo('/ideContri');

      Gerador.wGrupo('/' + SchemaStr);
    GerarRodape;
    {Assinar XML}
    XML := Assinar(Gerador.ArquivoFormatoXML, AnsiString(SchemaStr));
    {$IFDEF DEBUG}
    with TStringList.Create do
    try
       Text := XML;
       SaveToFile(IncludeTrailingPathDelimiter(TACBrReinf(FACBrReinf).Configuracoes.Arquivos.PathSalvar) + Copy(Self.ClassName, 2, Length( Self.ClassName ) ) +
                  '-'+ IntTostr(Dayof(Now)) + IntTostr(MonthOf(Now)) + IntTostr(YearOf(Now))+ '_'+ IntTostr(HourOf(Now))+ IntTostr(MinuteOf(Now))+IntTostr(SecondOf(Now)) + '_' +IntTostr(MilliSecondOf(Now)) + '.xml');
    finally
      Free;
    end;
    {$ENDIF}
    if AValidar then // evtInfoContribuinte-v1_01_01.xsd
      ValidarEventos(TReinfSchemaStr[ord(FSchema)] + PrefixVersao + Copy( TACBrReinf( FACBrReinf ).Versao, 2, ( Length( TACBrReinf( FACBrReinf ).Versao ) ) ) );
  except on e:exception do
    raise Exception.Create(e.Message);
  end;
  Result := (Gerador.ArquivoFormatoXML <> '');
end;

function TEventoReinf.GetXML: AnsiString;
begin
  if String(FXML) = EmptyStr then
    GerarXML;
  Result := FXML;
end;

procedure TEventoReinf.SaveToFile(const ACaminhoArquivo: string);
var
  lStr: TStringList;
  lFileName: string;
begin
  lFileName := ACaminhoArquivo;
  lStr:= TStringList.Create;
  try
    lStr.Text := String(XML);
    lStr.SaveToFile(ChangeFileExt(lFileName,'.xml'));
  finally
    lStr.Free;
  end;
end;

procedure TEventoReinf.SetSchema(ASchema: TReinfSchema);
begin
  FSchema := ASchema;
end;

procedure TEventoReinf.ValidarEventos(AEvento: String);
var
  Erro: string;
  AXML: AnsiString;
  EhValido: Boolean;
begin
  AXML := AnsiString(FXMLAssinado);

  if EstaVazio(String(AXML)) then
  begin
    Assinar(AXML, AnsiString(AEvento));
    AXML := AnsiString(FXMLAssinado);
  end;

  with TACBrReinf(FACBrReinf) do
  begin
    EhValido := SSL.Validar(string(AXML), Configuracoes.Arquivos.PathSchemas + AEvento + '.xsd', Erro);
    if not EhValido then
    begin
      FErros := ACBrStr('Falha na validação dos dados do evento: ') + AEvento + sLineBreak;
      FErros := FErros + sLineBreak + Erro;

      {$IFDEF DEBUG}
      with TStringList.Create do
      try
        Add(AXML);
        Add('<!--' + FErros + '-->');
        SaveToFile(Configuracoes.Arquivos.PathSalvar+AEvento+'_error' +'.xml');
      finally
        Free;
      end;
      {$ENDIF}
      raise EACBReinfException.Create(FErros);
    end;
  end;
end;

{ TReinf }

constructor TReinf.Create(AACBrReinf: TACBrDFe);
begin
  FACBrReinf := AACBrReinf;
end;

function TReinf.GerarChaveID(const ACNPJF: string; ASequencial: Integer; AOrgaoPublico: Boolean): string;
var
  nAno, nMes, nDia, nHora, nMin, nSeg, nMSeg: Word;
  AEmissao: TDateTime;
begin
  AEmissao := Now;
  // Se o usuario informar 0; o código numerico sera gerado de maneira aleatória //
  if ASequencial = 0 then
    ASequencial := Random(99999);
  DecodeDate(AEmissao, nAno, nMes, nDia);
  DecodeTime(AEmissao, nHora, nMin, nSeg, nMSeg);
  Result := 'ID';

  if (Length(ACNPJF) = 14) then
    Result := Result + IntToStr(1)
  else
    Result := Result + IntToStr(2);

  if AOrgaoPublico then
    Result := Result + copy(OnlyNumber(ACNPJF) + '00000000000000', 1, 14)
  else
    Result := Result + copy(OnlyNumber(Copy(ACNPJF, 1, 8)) + '00000000000000', 1, 14);
  Result := Result + IntToStrZero(nAno, 4);
  Result := Result + IntToStrZero(nMes, 2);
  Result := Result + IntToStrZero(nDia, 2);
  Result := Result + IntToStrZero(nHora, 2);
  Result := Result + IntToStrZero(nMin, 2);
  Result := Result + IntToStrZero(nSeg, 2);
  Result := Result + IntToStrZero(ASequencial, 5);
end;

function TReinf.Id(ASequencial: Integer): string;
begin
  if FId = EmptyStr then
    FId := GerarChaveID(TACBrReinf(FACBrReinf).ideContri.NrInsc, ASequencial);
  Result := FId;
end;

{ TEventoReinfs }

function TEventoReinfs.GetItem(Index: Integer): TEventoReinf;
begin
  Result := TEventoReinf(Inherited Items[Index]);
end;

function TEventoReinfs.New(AACBrReinf: TACBrDFe): TEventoReinf;
begin
  Result := TEventoReinf.Create(AACBrReinf);
  Add(Result);
end;

procedure TEventoReinfs.SetItem(Index: Integer; const Value: TEventoReinf);
begin
  Put(Index, Value);
end;

end.
