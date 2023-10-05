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
  IniFiles;

const
  CURL_SEDEX      = 'http://ws.correios.com.br/calculador/CalcPrecoPrazo.aspx?';
  CURL_SEDEXPrazo = 'http://ws.correios.com.br/calculador/CalcPrecoPrazo.asmx/CalcPrazo?';

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

  fRastreio := TACBrRastreioClass.Create;
  fRastreio.Clear;
  fsCodContrato := '';
  fsDsSenha := '';
  fsCepOrigem := '';
  fsCepDestino := '';
  fnVlPeso := 0;
  fnCdFormato := TpfCaixaPacote;
  fnVlComprimento := 0;
  fnVlAltura := 0;
  fnVlLargura := 0;
  fsMaoPropria := False;
  fnVlValorDeclarado := 0;
  fsAvisoRecebimento := False;
  fnCdServico := Tps04510PAC;
  fUrlConsulta := CURL_SEDEX;

  fCodigoServico := '';
  fValor := 0;
  fPrazoEntrega := 0;
  fValorSemAdicionais := 0;
  fValorMaoPropria := 0;
  fValorAvisoRecebimento := 0;
  fValorValorDeclarado := 0;
  fEntregaDomiciliar := '';
  fEntregaSabado := '';
  fErro := 0;
  fMsgErro := '';
  fDataMaxEntrega := '';
end;

destructor TACBrSedex.Destroy;
begin
  fRastreio.Free;
  inherited Destroy;
end;

function TACBrSedex.GetUrl(TpServico: String): String;
var
  Servico:integer;
begin
  try
    Servico := StrToInt(copy(TpServico,1,2));
    case Servico of
      4,40,81: Result:= CURL_SEDEX
      else
        Result:= CURL_SEDEXPrazo;
    end;
  except
    raise EACBrSedexException.CreateACBrStr('Erro ao buscar a URL');
  end;
end;

function TACBrSedex.Consultar: Boolean;
var
  TpServico, TpFormato, TpMaoPropria, TpAvisoRecebimento, Buffer: string;
begin
  case fnCdServico of
    Tps04510PAC :
      TpServico := '04510';
    Tps04014SEDEX :
      TpServico := '04014';
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
    else
      raise EACBrSedexException.CreateACBrStr('Tipo de Serviço Inválido');
  end;

  fUrlConsulta := GetUrl(TpServico);

  case fnCdFormato of
    TpfCaixaPacote :
      TpFormato := '1';
    TpfRoloPrisma :
      TpFormato := '2';
    TpfEnvelope :
      TpFormato := '3';
    else
      raise EACBrSedexException.CreateACBrStr('Formato da Embalagem Inválido');
  end;

  if fsMaoPropria then
    TpMaoPropria := 'S'
  else
    TpMaoPropria := 'N';

  if fsAvisoRecebimento then
    TpAvisoRecebimento := 'S'
  else
    TpAvisoRecebimento := 'N';

  try
    if fUrlConsulta = CURL_SEDEX then
      Self.HTTPGet(fUrlConsulta +
        'nCdEmpresa=' + fsCodContrato +
        '&sDsSenha=' + fsDsSenha +
        '&sCepOrigem=' + OnlyNumber(fsCepOrigem) +
        '&sCepDestino=' + OnlyNumber(fsCepDestino) +
        '&nVlPeso=' + FloatToString(fnVlPeso) +
        '&nCdFormato=' + TpFormato +
        '&nVlComprimento=' + FloatToString(fnVlComprimento) +
        '&nVlAltura=' + FloatToString(fnVlAltura) +
        '&nVlLargura=' + FloatToString(fnVlLargura) +
        '&sCdMaoPropria=' + TpMaoPropria +
        '&nVlValorDeclarado=' + FloatToString(fnVlValorDeclarado) +
        '&sCdAvisoRecebimento=' + TpAvisoRecebimento +
        '&nCdServico=' + TpServico +
        '&nVlDiametro=' + FloatToString(fnVlDiametro) +
        '&StrRetorno=xml')
    else
       Self.HTTPGet(fUrlConsulta +
        '&nCdServico=' + TpServico +
        '&sCepOrigem=' + OnlyNumber(fsCepOrigem) +
        '&sCepDestino=' + OnlyNumber(fsCepDestino)+
        '&StrRetorno=xml');
  except
    on E: Exception do
    begin
      raise EACBrSedexException.Create('Erro ao consultar Sedex' + sLineBreak + E.Message);
    end;
  end;

  //DEBUG
  //Self.RespHTTP.SaveToFile('C:\TEMP\CONSULTA.HTML');

  Buffer := Self.RespHTTP.Text;

  retCodigoServico         := LerTagXml(Buffer, 'Codigo', True);
  retPrazoEntrega          := StrToIntDef(LerTagXml(Buffer, 'PrazoEntrega', True),-1);
  if fUrlConsulta = CURL_SEDEX then
  begin
    retvalor                 := StringToFloatDef(LerTagXml(Buffer, 'Valor', True),-1);
    retValorSemAdicionais    := StringToFloatDef(LerTagXml(Buffer, 'ValorSemAdicionais', True),-1);
    retValorMaoPropria       := StringToFloatDef(LerTagXml(Buffer, 'ValorMaoPropria', True),-1);
    retValorAvisoRecebimento := StringToFloatDef(LerTagXml(Buffer, 'ValorAvisoRecebimento', True),-1);
    retValorValorDeclarado   := StringToFloatDef(LerTagXml(Buffer, 'ValorValorDeclarado', True),-1);
    retDataMaxEntrega        := '';
  end
  else
  begin
    retDataMaxEntrega        := LerTagXml(Buffer, 'DataMaxEntrega', True);
    retvalor                 := 0;
    retValorSemAdicionais    := 0;
    retValorMaoPropria       := 0;
    retValorAvisoRecebimento := 0;
    retValorValorDeclarado   := 0;
  end;
  retEntregaDomiciliar     := LerTagXml(Buffer, 'EntregaDomiciliar', True);
  retEntregaSabado         := LerTagXml(Buffer, 'EntregaSabado', True);
  retErro                  := StrToIntDef(LerTagXml(Buffer, 'Erro', True),-1);
  retMsgErro               := LerTagXml(Buffer, 'MsgErro', True);

  retMsgErro := StringReplace(retMsgErro, '<![CDATA[', '', [rfReplaceAll]);
  retMsgErro := StringReplace(retMsgErro, ']]>', '', [rfReplaceAll]);

  if fUrlConsulta = CURL_SEDEX then
    Result := (retErro = 0)
  else
    Result := (retErro = -1);
end;

//Código de erro Mensagem de erro
//0 Processamento com sucesso
//-1 Código de serviço inválido
//-2 CEP de origem inválido
//-3 CEP de destino inválido
//-4 Peso excedido
//-5 O Valor Declarado não deve exceder R$ 10.000,00
//-6 Serviço indisponível para o trecho informado
//-7 O Valor Declarado é obrigatório para este serviço
//-8 Este serviço não aceita Mão Própria
//-9 Este serviço não aceita Aviso de Recebimento
//-10 Precificação indisponível para o trecho informado
//-11 Para definição do preço deverão ser informados, também, o comprimento, a largura e altura do objeto em centímetros (cm).
//-12 Comprimento inválido.
//-13 Largura inválida.
//-14 Altura inválida.
//-15 O comprimento não pode ser maior que 105 cm.
//-16 A largura não pode ser maior que 105 cm.
//-17 A altura não pode ser maior que 105 cm.
//-18 A altura não pode ser inferior a 2 cm.
//-20 A largura não pode ser inferior a 11 cm.
//-22 O comprimento não pode ser inferior a 16 cm.
//-23 A soma resultante do comprimento + largura + altura não deve superar a 200 cm.
//-24 Comprimento inválido.
//-25 Diâmetro inválido
//-26 Informe o comprimento.
//-27 Informe o diâmetro.
//-28 O comprimento não pode ser maior que 105 cm.
//-29 O diâmetro não pode ser maior que 91 cm.
//-30 O comprimento não pode ser inferior a 18 cm.
//-31 O diâmetro não pode ser inferior a 5 cm.
//-32 A soma resultante do comprimento + o dobro do diâmetro não deve superar a 200 cm.
//-33 Sistema temporariamente fora do ar. Favor tentar mais tarde.
//-34 Código Administrativo ou Senha inválidos.
//-35 Senha incorreta.
//-36 Cliente não possui contrato vigente com os Correios.
//-37 Cliente não possui serviço ativo em seu contrato.
//-38 Serviço indisponível para este código administrativo.
//-39 Peso excedido para o formato envelope
//-40 Para definicao do preco deverao ser informados, tambem, o comprimento e a largura e altura do objeto em centimetros (cm).
//-41 O comprimento nao pode ser maior que 60 cm.
//-42 O comprimento nao pode ser inferior a 16 cm.
//-43 A soma resultante do comprimento + largura nao deve superar a 120 cm.
//-44 A largura nao pode ser inferior a 11 cm.
//-45 A largura nao pode ser maior que 60 cm.
//-888 Erro ao calcular a tarifa
//006 Localidade de origem não abrange o serviço informado
//007 Localidade de destino não abrange o serviço informado
//008 Serviço indisponível para o trecho informado
//009 CEP inicial pertencente a Área de Risco.
//010 CEP final pertencente a Área de Risco. A entrega será realizada, temporariamente, na agência mais próxima do endereço do destinatário.
//011 CEP inicial e final pertencentes a Área de Risco
//7 Serviço indisponível, tente mais tarde
//99 Outros erros diversos do .Net
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
    Self.HTTPGet('http://www.websro.com.br/detalhes.php?P_COD_UNI='+ ACodRastreio);
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
        LLocal := (RetornarConteudoEntre(LLinha, '<li>', '</li>'));

      if Pos('<li>Destino', LLinha) > 0 then
        LLocal := LLocal + #13#10 + (RetornarConteudoEntre(LLinha, '<li>', '</li>'));

      if Pos('>Status', LLinha) > 0 then
        LObservacoes := RetornarConteudoEntre(LLinha, '<b>', '</b>');

      LDeveCriar := Trim(LLocal) <> '';
      if LDeveCriar then
      begin
        LRastreio := retRastreio.New;
        LRastreio.DataHora   := StrToDateTime(LData);
        LRastreio.Local      := LLocal;
        LRastreio.Situacao   := LObservacoes;
        LRastreio.Observacao := LObservacoes;

        LData        := EmptyStr;
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
  IniSedex: TMemIniFile;
  Sessao: String;
begin
  IniSedex := TMemIniFile.Create('');
  try

    LerIniArquivoOuString(AIniSedex, IniSedex);
    with Self do
    begin
      Sessao := 'SEDEX';

      CepOrigem        := OnlyNumber(IniSedex.ReadString(Sessao,'CepOrigem',''));
      CepDestino       := OnlyNumber(IniSedex.ReadString(Sessao,'CepDestino',''));
      Servico          := TACBrTpServico(IniSedex.ReadInteger(Sessao,'Servico',0));
      Peso             := IniSedex.ReadFloat(Sessao,'Peso',0);
      Altura           := IniSedex.ReadFloat(Sessao,'Altura',0);
      Largura          := IniSedex.ReadFloat(Sessao,'Largura',0);
      Comprimento      := IniSedex.ReadFloat(Sessao,'Comprimento',0);
      Diametro         := IniSedex.ReadFloat(Sessao,'Diametro',0);
      ValorDeclarado   := IniSedex.ReadFloat(Sessao,'ValorDeclarado',0);
      Formato          := TACBrTpFormato(IniSedex.ReadInteger(Sessao,'Formato',0));
      AvisoRecebimento := IniSedex.ReadBool(Sessao,'AvisoRecebimento',False);
      MaoPropria       := IniSedex.ReadBool(Sessao,'MaoPropria',False);
    end;
  finally
    IniSedex.free;
    Result := True;
  end;
end;

end.