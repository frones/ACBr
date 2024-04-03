{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
|* Doação por "datilas" no link
|* http://www.projetoacbr.com.br/forum/index.php?/topic/17388-correios-calculo-de-sedex-pac/
******************************************************************************}

{$I ACBr.inc}

unit ACBrSedex;

interface

uses
  Classes, SysUtils,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$Else}
   Contnrs,
  {$IfEnd}
  ACBrBase, ACBrSocket,
  ACBrUtil.Strings,
  ACBrUtil.Base,
  ACBrUtil.XMLHTML,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBrJSON,
  IniFiles;

const
  URL_SEDEX    = 'https://www.cepcerto.com/ws/json-frete';
  URL_RASTREIO = 'http://www.websro.com.br/detalhes.php?P_COD_UNI=';
type
  TACBrTpServico = (Tps04510PAC, Tps04014SEDEX, Tps40215SEDEX10,
    Tps40290SEDEXHOJE, Tps81019eSEDEX, Tps44105MALOTE,
    Tps85480AEROGRAMA, Tps10030CARTASIMPLES, Tps10014CARTAREGISTRADA,
    Tps16012CARTAOPOSTAL, Tps20010IMPRESSO, Tps14010MALADIRETA,
    Tps04014SEDEXVarejo, Tps40045SEDEXaCobrarVarejo, Tps40215SEDEX10Varejo,
    Tps40290SEDEXHojeVarejo, Tps04510PACVarejo, Tps04669PACContrato, Tps04162SEDEXContrato, Tps20150IMPRESSOContrato,
    Tps03220SEDEXContrato,Tps03298PACContrato);

  TACBrTpFormato = (TpfCaixaPacote, TpfRoloPrisma, TpfEnvelope);

  { EACBrSedexException }

  EACBrSedexException = class(Exception)
  public
    constructor CreateACBrStr(const msg : string);
  end ;

  { TACBrRastreio }

  TACBrRastreio = class
  private
    fDataHora: TDateTime;
    fLocal: string;
    fSituacao: string;
    fObservacao: string;
  public
    property DataHora: TDateTime read fDataHora write fDataHora;
    property Local: String read fLocal write fLocal;
    property Situacao: String read fSituacao write fSituacao;
    property Observacao: String read fObservacao write fObservacao;
  end;

  { TACBrRastreioClass }

  TACBrRastreioClass = class(TObjectList{$IfDef HAS_SYSTEM_GENERICS}<TACBrRastreio>{$EndIf})
  protected
    procedure SetObject(Index: integer; Item: TACBrRastreio);
    function GetObject(Index: integer): TACBrRastreio;
  public
    function Add(Obj: TACBrRastreio): integer;
    function New: TACBrRastreio;
    property Objects[Index: integer]: TACBrRastreio read GetObject write SetObject;
      default;
  end;

  { TACBrSedex }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrSedex = class(TACBrHTTP)
  private
    fsCodContrato: String;
    fsDsSenha: String;
    fsCepOrigem: String;
    fsCepDestino: String;
    fnVlPeso: Double;
    fnCdFormato: TACBrTpFormato;
    fnVlComprimento: Double;
    fnVlAltura: Double;
    fnVlLargura: Double;
    fsMaoPropria: Boolean;
    fnVlValorDeclarado: Double;
    fsAvisoRecebimento: Boolean;
    fnCdServico: TACBrTpServico;
    fnVlDiametro: Double;
    fUrlConsulta: String;

    fCodigoServico: String;
    fValor: Double;
    fPrazoEntrega: Integer;
    fValorSemAdicionais: Double;
    fValorMaoPropria: Double;
    fValorAvisoRecebimento: Double;
    fValorValorDeclarado: Double;
    fEntregaDomiciliar: String;
    fEntregaSabado: String;
    fErro: Integer;
    fMsgErro: String;
    fRastreio: TACBrRastreioClass;
    fDataMaxEntrega: String;
    Function GetUrl(TpServico: String):String;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Consultar: Boolean;
    procedure Rastrear(const ACodRastreio: String);

    function LerArqIni(const AIniSedex: String): Boolean;

    property retCodigoServico: String read fCodigoServico write fCodigoServico;
    property retValor: Double read fValor write fValor;
    property retPrazoEntrega: Integer read fPrazoEntrega write fPrazoEntrega;
    property retValorSemAdicionais: Double read fValorSemAdicionais
      write fValorSemAdicionais;
    property retValorMaoPropria: Double read fValorMaoPropria write fValorMaoPropria;
    property retValorAvisoRecebimento: Double
      read fValorAvisoRecebimento write fValorAvisoRecebimento;
    property retValorValorDeclarado: Double
      read fValorValorDeclarado write fValorValorDeclarado;
    property retEntregaDomiciliar: String read fEntregaDomiciliar
      write fEntregaDomiciliar;
    property retEntregaSabado: String read fEntregaSabado write fEntregaSabado;
    property retErro: Integer read fErro write fErro;
    property retMsgErro: String read fMsgErro write fMsgErro;
    property retDataMaxEntrega: String read fDataMaxEntrega write fDataMaxEntrega;
    property retRastreio: TACBrRastreioClass read fRastreio write fRastreio;

  published
    property CodContrato: String read fsCodContrato write fsCodContrato;
    property Senha: String read fsDsSenha write fsDsSenha;
    property CepOrigem: String read fsCepOrigem write fsCepOrigem;
    property CepDestino: String read fsCepDestino write fsCepDestino;
    property Peso: Double read fnVlPeso write fnVlPeso;
    property Formato: TACBrTpFormato read fnCdFormato write fnCdFormato;
    property Comprimento: Double read fnVlComprimento write fnVlComprimento;
    property Altura: Double read fnVlAltura write fnVlAltura;
    property Largura: Double read fnVlLargura write fnVlLargura;
    property MaoPropria: Boolean read fsMaoPropria write fsMaoPropria
      default false;
    property ValorDeclarado: Double read fnVlValorDeclarado write fnVlValorDeclarado;
    property AvisoRecebimento: Boolean read fsAvisoRecebimento write fsAvisoRecebimento
      default false;
    property Servico: TACBrTpServico read fnCdServico write fnCdServico;
    property Diametro: Double read fnVlDiametro write fnVlDiametro;
    property UrlConsulta: String read fUrlConsulta write fUrlConsulta;
  end;


implementation

{ EACBrSedexException }

constructor EACBrSedexException.CreateACBrStr(const msg: string);
begin
  inherited Create( ACBrStr(msg) );
end;

{ TACBrRastreioClass }

procedure TACBrRastreioClass.SetObject(Index: integer; Item: TACBrRastreio);
begin
  inherited Items[Index] := Item;
end;

function TACBrRastreioClass.GetObject(Index: integer): TACBrRastreio;
begin
  Result := TACBrRastreio(inherited Items[Index]);
end;

function TACBrRastreioClass.New: TACBrRastreio;
begin
  Result := TACBrRastreio.Create;
  Add(Result);
end;

function TACBrRastreioClass.Add(Obj: TACBrRastreio): integer;
begin
  Result := inherited Add(Obj);
end;

{ TACBrSedex }

constructor TACBrSedex.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fRastreio              := TACBrRastreioClass.Create;
  fRastreio.Clear;
  fsCodContrato          := '';
  fsDsSenha              := '';
  fsCepOrigem            := '';
  fsCepDestino           := '';
  fnVlPeso               := 0;
  fnCdFormato            := TpfCaixaPacote;
  fnVlComprimento        := 0;
  fnVlAltura             := 0;
  fnVlLargura            := 0;
  fsMaoPropria           := False;
  fnVlValorDeclarado     := 0;
  fsAvisoRecebimento     := False;
  fnCdServico            := Tps04510PAC;
  fUrlConsulta           := URL_SEDEX;

  fCodigoServico         := '';
  fValor                 := 0;
  fPrazoEntrega          := 0;
  fValorSemAdicionais    := 0;
  fValorMaoPropria       := 0;
  fValorAvisoRecebimento := 0;
  fValorValorDeclarado   := 0;
  fEntregaDomiciliar     := '';
  fEntregaSabado         := '';
  fErro                  := 0;
  fMsgErro               := '';
  fDataMaxEntrega        := '';
end;

destructor TACBrSedex.Destroy;
begin
  fRastreio.Free;
  inherited Destroy;
end;

function TACBrSedex.GetUrl(TpServico: String): String;
begin
  Result:= URL_SEDEX;
end;

function TACBrSedex.Consultar: Boolean;
  function parametros(const AParametro : String):String;
  begin
    result :=  '/' + OnlyNumber(AParametro);
  end;

  var
    TpServico, TpFormato, TpMaoPropria, TpAvisoRecebimento, Buffer: string;

var
  Index: integer;
  LErro, LResposta : String;
  LRastreio : TACBrRastreio;
  LJson : TACBrJSONObject;
begin
  case fnCdServico of
     Tps04510PAC :
      TpServico := '04510';
    Tps04014SEDEX :
      TpServico := '04014';
    (*
    Tps40215SEDEX10 :
      TpServico := '40215';
    Tps40290SEDEXHOJE :
      TpServico := '40290';
    Tps81019eSEDEX :
      TpServico := '81019';
    Tps44105MALOTE :
      TpServico := '44105';
    Tps85480AEROGRAMA :
      TpServico := '85480';
    Tps10030CARTASIMPLES :
      TpServico := '10030';
    Tps10014CARTAREGISTRADA :
      TpServico := '10014';
    Tps16012CARTAOPOSTAL :
      TpServico := '16012';
    Tps20010IMPRESSO :
      TpServico := '20010';
    Tps14010MALADIRETA :
      TpServico := '14010';
    Tps04014SEDEXVarejo :
      TpServico := '04014';
    Tps40045SEDEXaCobrarVarejo :
      TpServico := '40045';
    Tps40215SEDEX10Varejo :
      TpServico := '40215';
    Tps40290SEDEXHojeVarejo :
      TpServico := '40290';
    Tps04510PACVarejo :
      TpServico := '04510';
    Tps04669PACContrato:
      TpServico := '04669';
    Tps04162SEDEXContrato:
      TpServico := '04162';
    Tps20150IMPRESSOContrato:
      TpServico := '20150';
    Tps03220SEDEXContrato:
      TpServico := '03220';
    Tps03298PACContrato:
      TpServico := '03298';
    *)
    else
      raise EACBrSedexException.CreateACBrStr('Tipo de Serviço Inválido ou desativado pelo provedor, valido somente 04510 PAC / 04014 Sedex');
  end;

  fUrlConsulta := GetUrl(TpServico);
  try
    Self.HTTPGet(fUrlConsulta +
               parametros(fsCepOrigem)   +
               parametros(fsCepDestino)  +
               parametros(FloatToString(fnVlPeso, '.', '0.000'))  +
               parametros(FloatToString(fnVlAltura))  +
               parametros(FloatToString(fnVlLargura))  +
               parametros(FloatToString(fnVlComprimento)) +
               '/'+Self.Senha
              );
  except on E: Exception do
    raise EACBrSedexException.Create('Erro ao consultar Sedex' + sLineBreak + E.Message);
  end;

  LResposta := Self.RespHTTP.Text;

  LJson := TACBrJSONObject.Parse(NativeStringToUTF8(LResposta));
  try
    if LJson.AsString['msg'] <> '' then
      raise EACBrSedexException.Create(LJson.AsString['msg']);

    if LJson.AsString['ErroHash'] <> '' then
      raise EACBrSedexException.Create(LJson.AsString['ErroHash']);

    if LJson.AsString['Erro'] <> '' then
      raise EACBrSedexException.Create(LJson.AsString['Erro']);

    case fnCdServico of
      Tps04510PAC :
        begin
          retvalor                 := StrToFloat(Trim(LJson.AsString['valorpac']));
          retDataMaxEntrega        := Trim(LJson.AsString['prazopac']);
        end;
      Tps04014SEDEX :
        begin
          retvalor                 := StrToFloat(Trim(LJson.AsString['valorsedex']));
          retDataMaxEntrega        := Trim(LJson.AsString['prazosedex']);
        end;
    end;
    Result := True;
  finally
    LJson.Free;
  end;
end;

procedure TACBrSedex.Rastrear(const ACodRastreio: String);
var
  LLista: TStringList;
  Index: integer;
  LObservacoes, LErro, LData, LHora, LLocal, LLinha: String;
  LDeveCriar: Boolean;
  LRastreio : TACBrRastreio;
begin
  retRastreio.Clear;

  if Length(ACodRastreio) <> 13 then
    raise EACBrSedexException.CreateACBrStr('Código de rastreamento deve conter 13 caracteres');

  try
    Self.HTTPGet(URL_RASTREIO + ACodRastreio);
  except
    on E: Exception do
    begin
      raise EACBrSedexException.Create('Erro ao Rastrear' + sLineBreak + E.Message);
    end;
  end;

  if Pos(ACBrStr('tente novamente mais tarde'), Self.RespHTTP.Text) > 0 then
  begin
    LErro := Trim(StripHTML(Self.RespHTTP.Text));
    LErro := Trim(StringReplace(LErro,'SRO - Internet','',[rfReplaceAll]));
    LErro := Trim(StringReplace(LErro,'Resultado da Pesquisa','',[rfReplaceAll]));

    raise EACBrSedexException.Create(LErro);
  end;

  if Pos(ACBrStr('O rastreamento não está disponível no momento:'), Self.RespHTTP.Text) > 0 then
     raise EACBrSedexException.Create('O rastreamento não está disponível no momento: '  + sLineBreak +
                                      ' - Verifique se o código do objeto está correto ' + sLineBreak +
                                      ' - O objeto pode demorar até 24 horas (após postagem) para ser rastreado no sistema do Correios.');

  LLista := TStringList.Create;
  try
    LLista.Text := Self.RespHTTP.Text;
    LDeveCriar := False;
    for Index := 0 to Pred(LLista.Count) do
    begin
      LLinha := Trim(LLista.Strings[Index]);
      if Pos('>Data', LLinha) > 0 then
      begin
        LData :=(RetornarConteudoEntre(LLinha, '>Data  : ', ' |'));
        LHora :=(RetornarConteudoEntre(LLinha, 'Hora: ', '</li>'));
        LData := LData + ' ' + LHora;
      end;

      if Pos('>Local', LLinha) > 0 then
        LLocal := (RetornarConteudoEntre(LLinha, '<li>', '</li>'));

      if Pos('<li>Origem', LLinha) > 0 then
        LLocal := LLocal +(RetornarConteudoEntre(LLinha, '<li>', '</li>'));

      if Pos('<li>Destino', LLinha) > 0 then
        LLocal := LLocal + #13#10 + (RetornarConteudoEntre(LLinha, '<li>', '</li>'));

      if Pos('>Status', LLinha) > 0 then
        LObservacoes := RetornarConteudoEntre(LLinha, '<b>', '</b>');

      LDeveCriar := Trim(LLocal) <> '';
      if LDeveCriar then
      begin
        LRastreio := retRastreio.New;
        LRastreio.DataHora   := StrToDateTimeDef(LData,0);
        LRastreio.Local      := LLocal;
        LRastreio.Situacao   := LObservacoes;
        LRastreio.Observacao := LObservacoes;

        //LData        := EmptyStr;
        LLocal       := EmptyStr;
        LObservacoes := EmptyStr;
        LDeveCriar   := False;
      end;
    end;
  finally
    LLista.Free;
  end;
end;

function TACBrSedex.LerArqIni(const AIniSedex: String): Boolean;
var
  LArqIni: TMemIniFile;
  LSessao: String;
begin
  LArqIni := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniSedex, LArqIni);

    LSessao := 'SEDEX';

    CepOrigem        := OnlyNumber(LArqIni.ReadString(LSessao,'CepOrigem',''));
    CepDestino       := OnlyNumber(LArqIni.ReadString(LSessao,'CepDestino',''));
    Servico          := TACBrTpServico(LArqIni.ReadInteger(LSessao,'Servico',0));
    Peso             := LArqIni.ReadFloat(LSessao,'Peso',0);
    Altura           := LArqIni.ReadFloat(LSessao,'Altura',0);
    Largura          := LArqIni.ReadFloat(LSessao,'Largura',0);
    Comprimento      := LArqIni.ReadFloat(LSessao,'Comprimento',0);
    Diametro         := LArqIni.ReadFloat(LSessao,'Diametro',0);
    ValorDeclarado   := LArqIni.ReadFloat(LSessao,'ValorDeclarado',0);
    Formato          := TACBrTpFormato(LArqIni.ReadInteger(LSessao,'Formato',0));
    AvisoRecebimento := LArqIni.ReadBool(LSessao,'AvisoRecebimento',False);
    MaoPropria       := LArqIni.ReadBool(LSessao,'MaoPropria',False);

  finally
    LArqIni.free;
    Result := True;
  end;
end;

end.
