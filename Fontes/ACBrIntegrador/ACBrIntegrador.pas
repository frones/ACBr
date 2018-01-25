{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                             }

{ Direitos Autorais Reservados (c) 2015 Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }

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

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }

{******************************************************************************}

{$I ACBr.inc}

unit ACBrIntegrador;

interface

uses
{$IFDEF MSWINDOWS}
 {$IFNDEF FPC}
  Windows,
 {$ENDIF}
{$ENDIF}
  Classes, SysUtils,
  pcnGerador, pcnLeitor, pcnVFPe, pcnVFPeW,
  ACBrBase;

const
  cACBrIntgerador_Versao = '0.1.0' ;

type
  EComandoIntegradorException = class( Exception );
  EIntegradorException = class( Exception );

  TACBrIntegrador = class;

  { TComandoIntegrador }
  TComandoIntegrador = class
  private
    FOwner: TACBrIntegrador;
    FLeitor: TLeitor;
    FPastaInput: String;
    FPastaOutput: String;
    FTimeout: Integer;
    FErro: String;
    FErroTimeout: Boolean;
    FResposta: String;
    procedure SetPastaInput(AValue: String);
    procedure SetPastaOutput(AValue: String);

  private
    function PegaResposta(Resp : String) : String;
    function AguardaArqResposta(numeroSessao: Integer) : String;
    procedure DoException( AMessage: String );

  public
    constructor Create( AOwner: TACBrIntegrador );
    destructor Destroy; override;
    procedure Clear;

    function EnviaComando(numeroSessao: Integer; Nome, Comando : String; TimeOutComando : Integer = 0) : String;
  public
    property PastaInput  : String  read FPastaInput  write SetPastaInput;
    property PastaOutput : String  read FPastaOutput write SetPastaOutput;
    property Timeout     : Integer read FTimeout     write FTimeout default 30;
    property ErroTimeout : Boolean read FErroTimeout;
    property Erro        : String  read FErro;
    property Resposta    : String  read FResposta;
  end;

  TACBrIntegradorGetNumeroSessao = procedure(var NumeroSessao: Integer) of object ;

  { TACBrIntegrador }

  TACBrIntegrador = class(TACBrComponent)
  private
    FGerador: TGerador;
    FComandoIntegrador: TComandoIntegrador;
    FNomeMetodo: String;
    FNomeComponente: String;
    FNumeroSessao: Integer;
    FOnGetNumeroSessao: TACBrIntegradorGetNumeroSessao;
    FParametro: TParametro;
    FMetodo: TMetodo;
    FParametros: TStringList;
    FRespostas: TStringList;
    FArqLOG: String;
    FOnGravarLog: TACBrGravarLog;

    function GetErroTimeout: Boolean;
    function GetPastaInput: String;
    function GetPastaOutput: String;
    function GetTimeout: Integer;
    procedure SetPastaInput(AValue: String);
    procedure SetPastaOutput(AValue: String);
    procedure SetTimeout(AValue: Integer);

  private
    function GerarArquivo: String;
    function GetAbout: String;
    function GetErroResposta: String;
    function GetNumeroSessao: Integer;
    function GetUltimaResposta: String;
    procedure GravaLog(AString : AnsiString ) ;
    procedure SetAbout(AValue: String);
    procedure DoException( AMessage: String );

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure DoLog(AString : String ) ;

    function Enviar(AdicionarNumeroSessao: Boolean = True): String;
    property Respostas: TStringList read FRespostas;
    property ErroResposta: String read GetErroResposta;

    property NomeComponente: String read FNomeComponente write FNomeComponente;
    property NomeMetodo: String read FNomeMetodo write FNomeMetodo;
    property Parametros: TStringList read FParametros;

    property NumeroSessao: Integer read GetNumeroSessao;
    function GerarNumeroSessao: Integer;
    procedure SetNomeMetodo(NomeMetodo: String; Homologacao: Boolean);

    property UltimaResposta: String read GetUltimaResposta;

    function EnviarPagamento(Pagamento: TEnviarPagamento): TRespostaPagamento;
    function EnviarStatusPagamento(StatusPagamento: TStatusPagamento): TRespostaStatusPagamento;
    function VerificarStatusValidador(AVerificarStatusValidador: TVerificarStatusValidador):
      TRespostaVerificarStatusValidador;
    function RespostaFiscal(ARespostaFiscal: TRespostaFiscal): TRetornoRespostaFiscal;
    function ConsultarNumeroSessaoIntegrador(ANumeroSessao: Integer) : String;

  published
    property About : String read GetAbout write SetAbout stored False ;
    property ArqLOG : String read FArqLOG write FArqLOG ;
    property OnGravarLog : TACBrGravarLog read FOnGravarLog write FOnGravarLog;

    property PastaInput  : String  read GetPastaInput    write SetPastaInput;
    property PastaOutput : String  read GetPastaOutput   write SetPastaOutput;
    property Timeout     : Integer read GetTimeout       write SetTimeout default 30;
    property OnGetNumeroSessao : TACBrIntegradorGetNumeroSessao read FOnGetNumeroSessao
       write FOnGetNumeroSessao;

    property ErroTimeout : Boolean read GetErroTimeout;
  end;


implementation

Uses
  dateutils, strutils,
  pcnConversao,
  ACBrUtil;

{ TComandoIntegrador }

constructor TComandoIntegrador.Create(AOwner: TACBrIntegrador);
begin
  FOwner := AOwner;
  FLeitor := TLeitor.Create;

  FPastaInput  := 'C:\Integrador\Input\';
  FPastaOutput := 'C:\Integrador\Output\';
  FTimeout     := 30;
  FErroTimeout := False;
end;

destructor TComandoIntegrador.Destroy;
begin
  FLeitor.Free;
  inherited Destroy;
end;

procedure TComandoIntegrador.Clear;
begin
  FErro := '';
  FErroTimeout := False;
  FResposta := '';
end;

procedure TComandoIntegrador.SetPastaInput(AValue: String);
begin
  if FPastaInput = AValue then Exit;
  FPastaInput := PathWithDelim(AValue);
end;

procedure TComandoIntegrador.SetPastaOutput(AValue: String);
begin
  if FPastaOutput = AValue then Exit;
  FPastaOutput := PathWithDelim(AValue);
end;

function TComandoIntegrador.EnviaComando(numeroSessao: Integer; Nome, Comando: String; TimeOutComando : Integer = 0): String;
var
  LocTimeOut, ActualTime, TimeToRetry : TDateTime;
  NomeArquivoXml, RespostaIntegrador : String;
  ATimeout: Integer;

  function CriarXml( NomeArquivo, Comando: String): String;
  var
    NomeArquivoTmp, NomeArquivoXml: String;
  begin
    NomeArquivoTmp := ChangeFileExt(NomeArquivo, '.tmp');
    FOwner.DoLog('Criando arquivo: '+NomeArquivoTmp);
    WriteToFile(NomeArquivoTmp, Comando);

    if not FileExists(NomeArquivoTmp) then
      DoException('Erro ao criar o arquivo: '+NomeArquivoTmp);

    NomeArquivoXml := ChangeFileExt(NomeArquivoTmp,'.xml');
    FOwner.DoLog('Renomeando arquivo: '+NomeArquivoTmp+' para: '+NomeArquivoXml);
    if not RenameFile(NomeArquivoTmp, NomeArquivoXml) then
      DoException('Erro ao renomear o arquivo: '+ NomeArquivoTmp+' para: '+NomeArquivoXml);

    Result := NomeArquivoXml;
  end;

begin
  Result := '';
  Clear;

  NomeArquivoXml := CriarXml( FPastaInput + LowerCase(Nome) + '-' + IntToStr(numeroSessao),
                              Comando);
  ActualTime  := Now;
  TimeToRetry := IncSecond(ActualTime,5);
  if (TimeOutComando > 0) then
    ATimeout := TimeOutComando
  else
    ATimeout := FTimeout;

  if (ATimeout <= 0) then
    ATimeout := 30;

  LocTimeOut := IncSecond(ActualTime, ATimeout);

  RespostaIntegrador := AguardaArqResposta(numeroSessao);
  while EstaVazio(RespostaIntegrador) and (ActualTime < LocTimeOut) do
  begin
    Sleep(100);
    RespostaIntegrador := AguardaArqResposta(numeroSessao);
    ActualTime := Now;
    if ActualTime > TimeToRetry then //Caso arquivo ainda não tenha sido consumido após 5 segundos, recria o arquivo
    begin
      TimeToRetry := IncSecond(ActualTime,5);
      if FilesExists(NomeArquivoXml) then
      begin
        try
          FOwner.DoLog('Apagando arquivo não processado: '+NomeArquivoXml);
          DeleteFile(NomeArquivoXml);
        except
        end;

        NomeArquivoXml := CriarXml( FPastaInput + LowerCase(Nome) +'-'+ IntToStr(numeroSessao) +
                                    '-' + FormatDateTime('HHNNSS', ActualTime),
                                    Comando);
      end;
    end;
  end;

  if FilesExists(NomeArquivoXml) then  // Apaga arquivo não tratado pelo Integrador
  begin
    FOwner.DoLog('Apagando arquivo: '+NomeArquivoXml);
    DeleteFile(NomeArquivoXml);
  end;

  if EstaVazio(RespostaIntegrador) then
  begin
    FErroTimeout := True;
    DoException('Sem Resposta do Integrador');
  end;

  FOwner.DoLog('RespostaIntegrador: '+RespostaIntegrador);
  FResposta:= RespostaIntegrador;
  Result := PegaResposta(RespostaIntegrador);
end;

function TComandoIntegrador.PegaResposta(Resp: String): String;
begin
  Result := '';
  FLeitor.Arquivo := Resp;
  if FLeitor.rExtrai(1, 'retorno') <> '' then
    Result := FLeitor.rCampo(tcStr, 'retorno')
  else if FLeitor.rExtrai(1, 'Resposta') <> '' then
    Result := FLeitor.rCampo(tcStr, 'Resposta');

  if FLeitor.rExtrai(1, 'Erro') <> '' then
    FErro := FLeitor.Grupo;

  if EstaVazio(Result) and EstaVazio(FErro) then
    Result := Resp;
end;

function TComandoIntegrador.AguardaArqResposta(numeroSessao: Integer): String;
var
  SL, SLArqResp : TStringList;
  I, J, MaxTentativas : Integer;
  Erro : Boolean;
  Arquivo: String;
begin
  FOwner.DoLog(DateTimeToStr(Now)+' - AguardaArqResposta, sessao: '+IntToStr(numeroSessao));

  Result := '';
  SL := TStringList.Create;
  SLArqResp := TStringList.Create;
  try
    SLArqResp.Clear;
    FindFiles(PathWithDelim(FPastaOutput)+'*.xml',SLArqResp);
    Sleep(50); //Tentar evitar ler arquivo enquanto está sendo escrito

    for I:=0  to SLArqResp.Count-1 do
    begin
      SL.Clear;

      try
        SL.LoadFromFile(SLArqResp[I]); //ERRO: Unable to open
        Arquivo := SL.Text;
      except
        J := 0;
        MaxTentativas := 5;
        while J < MaxTentativas do
        begin
          try
            Erro := False;
            Sleep(500);
            SL.LoadFromFile(SLArqResp[I]); //ERRO: Unable to open
            Arquivo := SL.Text;
          except
            Erro := True;
            if J = (MaxTentativas-1) then
              Arquivo := ''; //Caso não consigo abrir, retorna vazio
          end;
          if not Erro then
            Break;
          Inc(J);
        end;
      end;

      FLeitor.Arquivo := Arquivo;
      if FLeitor.rExtrai(1, 'Identificador') <> '' then
      begin
        if FLeitor.rCampo(tcInt, 'Valor') = numeroSessao then
        begin
          Result := Trim(FLeitor.Arquivo);
          DeleteFile(SLArqResp[I]);
          Exit;
        end;
      end;
    end;
  finally
    SLArqResp.Free;
    SL.Free;
  end;
end;

procedure TComandoIntegrador.DoException(AMessage: String);
begin
  FOwner.DoLog('EComandoIntegradorException: '+AMessage);
  raise EComandoIntegradorException.Create(AMessage);
end;

{ TACBrIntegrador }

constructor TACBrIntegrador.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FOnGetNumeroSessao := Nil;
  FGerador := TGerador.Create;
  FComandoIntegrador := TComandoIntegrador.Create(Self);
  FParametro := TParametro.Create(FGerador);
  FMetodo := TMetodo.Create(FGerador);
  FParametros := TStringList.Create;
  FRespostas := TStringList.Create;

  Clear;
end;

destructor TACBrIntegrador.Destroy;
begin
  FRespostas.Free;
  FParametros.Free;
  FMetodo.Free;
  FParametro.Free;
  FComandoIntegrador.Free;
  FGerador.Free;

  inherited Destroy;
end;

procedure TACBrIntegrador.Clear;
begin
  FNumeroSessao := 0;
  FNomeMetodo := '';
  FNomeComponente := '';
  FParametros.Clear;
end;

procedure TACBrIntegrador.DoLog(AString: String);
var
  Tratado: Boolean;
begin
  Tratado := False;
  if Assigned( FOnGravarLog ) then
    FOnGravarLog( AString, Tratado );

  if not Tratado then
    GravaLog( AString );
end;

function TACBrIntegrador.GetErroTimeout: Boolean;
begin
  Result := FComandoIntegrador.ErroTimeout;
end;

function TACBrIntegrador.GetPastaInput: String;
begin
  Result := FComandoIntegrador.PastaInput;
end;

procedure TACBrIntegrador.SetPastaInput(AValue: String);
begin
  FComandoIntegrador.PastaInput := AValue;
end;

function TACBrIntegrador.GetPastaOutput: String;
begin
  Result := FComandoIntegrador.PastaOutput;
end;

procedure TACBrIntegrador.SetPastaOutput(AValue: String);
begin
  FComandoIntegrador.PastaOutput := AValue;
end;

function TACBrIntegrador.GetTimeout: Integer;
begin
  Result := FComandoIntegrador.Timeout;
end;

procedure TACBrIntegrador.SetTimeout(AValue: Integer);
begin
  FComandoIntegrador.Timeout := AValue;
end;

function TACBrIntegrador.Enviar(AdicionarNumeroSessao: Boolean): String;
Var
  DadosIntegrador, NomeArq, ErroIntegrador: String;
begin
  FRespostas.Clear;
  FComandoIntegrador.Clear;

  GerarNumeroSessao;

  if AdicionarNumeroSessao then
    FParametros.Insert(0, 'numeroSessao='+IntToStr(FNumeroSessao) );

  DadosIntegrador := GerarArquivo;

  if (FNomeMetodo = '') then
    DoException('NomeMetodo não definido');

  NomeArq := FNomeMetodo+'-'+FormatDateTime('yyyymmddhhnnss', Now);
  DoLog( 'Sessão: '+IntToStr(FNumeroSessao)+', Dados: '+DadosIntegrador);

  Result := FComandoIntegrador.EnviaComando( FNumeroSessao, NomeArq, DadosIntegrador );
  DoLog( 'Sessão: '+IntToStr(FNumeroSessao)+', Resposta: '+Result);

  if EstaVazio(Result) then
  begin
    ErroIntegrador := FComandoIntegrador.Erro;
    if EstaVazio(ErroIntegrador) then
      ErroIntegrador := 'Sem resposta do Integrador';

    DoException(ErroIntegrador)
  end;

  if (LeftStr(Result,2) = '\"') then
    Delete(Result,1,2);
    
  FRespostas.Delimiter := '|';
  {$IFDEF FPC}
   FRespostas.StrictDelimiter := True;
   FRespostas.DelimitedText   := Result;
  {$ELSE}
   FRespostas.DelimitedText :=
           '"' + StringReplace(Result, FRespostas.Delimiter,
                            '"' + FRespostas.Delimiter + '"', [rfReplaceAll]) +
           '"';
  {$ENDIF}
end ;

function TACBrIntegrador.GetErroResposta: String;
begin
  Result := FComandoIntegrador.Erro;
end;

function TACBrIntegrador.GerarArquivo: String;
var
  I: Integer;
  ParseCMD : Boolean;
  Param: String;
begin
  Result := '';
  FGerador.LayoutArquivoTXT.Clear;
  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(FNumeroSessao, FNomeComponente, FNomeMetodo);

  for I := 0 to FParametros.Count-1 do
  begin
    Param := FParametros.ValueFromIndex[I];
    ParseCMD := (Pos('<![CDATA[',Param) <= 0);
    FParametro.GerarParametro( FParametros.Names[I], Param , tcStr, ParseCMD);
  end;

  FMetodo.FinalizarMetodo;

  Result := FGerador.ArquivoFormatoXML;
end;

function TACBrIntegrador.GetAbout: String;
begin
  Result := 'ACBrIntegrador Ver: '+cACBrIntgerador_Versao;
end;


procedure TACBrIntegrador.GravaLog(AString: AnsiString);
begin
  if (ArqLOG = '') then
    Exit;

  WriteLog( ArqLOG, FormatDateTime('dd/mm/yy hh:nn:ss:zzz',now) + ' - ' + AString );
end;

procedure TACBrIntegrador.SetAbout(AValue: String);
begin
  {}
end;

procedure TACBrIntegrador.DoException(AMessage: String);
begin
  DoLog('EIntegradorException: '+AMessage);
  raise EIntegradorException.Create(ACBrStr(AMessage));
end;

function TACBrIntegrador.GerarNumeroSessao: Integer;
var
  Sessao : Integer;
begin
  Sessao := Random(999999);
  FNumeroSessao := Sessao;

  if Assigned( FOnGetNumeroSessao ) then
     FOnGetNumeroSessao( FNumeroSessao ) ;

  if FNumeroSessao <= 0 then
    FNumeroSessao := Sessao;

  Result := FNumeroSessao;
end;

function TACBrIntegrador.GetNumeroSessao: Integer;
begin
  //if Assigned( FOnGetNumeroSessao ) then
  //   FOnGetNumeroSessao( FNumeroSessao ) ;

  Result := FNumeroSessao;
end;

function TACBrIntegrador.GetUltimaResposta: String;
begin
  Result := FComandoIntegrador.Resposta;
end;

procedure TACBrIntegrador.SetNomeMetodo(NomeMetodo: String; Homologacao: Boolean
  );
begin
  FNomeMetodo := IfThen(Homologacao,'H','')+NomeMetodo;
end;

function TACBrIntegrador.EnviarPagamento(Pagamento: TEnviarPagamento
  ): TRespostaPagamento;
var
  Comando, Resp : String;
begin
{$IFNDEF COMPILER23_UP}
  Result := Nil;
{$ENDIF}
  GerarNumeroSessao;

  Pagamento.Identificador := numeroSessao;
  Comando := Pagamento.AsXMLString;
  DoLog('EnviarPagamento( '+Comando+' )');

  Resp := FComandoIntegrador.EnviaComando( numeroSessao, 'EnviarPagamento', Comando);

  Result := TRespostaPagamento.Create;
  Result.AsXMLString := Resp;
end;

function TACBrIntegrador.EnviarStatusPagamento(
  StatusPagamento: TStatusPagamento): TRespostaStatusPagamento;
var
  Comando, Resp : String;
begin
{$IFNDEF COMPILER23_UP}
  Result := Nil;
{$ENDIF}
  GerarNumeroSessao;

  StatusPagamento.Identificador := numeroSessao;
  Comando := StatusPagamento.AsXMLString;
  DoLog('EnviarStatusPagamento( '+Comando+' )');

  Resp := FComandoIntegrador.EnviaComando(numeroSessao,'EnviarStatusPagamento',Comando);

  Result := TRespostaStatusPagamento.Create;
  Result.AsXMLString := Resp;
end;

function TACBrIntegrador.VerificarStatusValidador(
  AVerificarStatusValidador: TVerificarStatusValidador
  ): TRespostaVerificarStatusValidador;
var
  Comando, Resp : String;
begin
{$IFNDEF COMPILER23_UP}
  Result := Nil;
{$ENDIF}
  GerarNumeroSessao;

  AVerificarStatusValidador.Identificador := numeroSessao;
  Comando := AVerificarStatusValidador.AsXMLString;
  DoLog('VerificarStatusValidador( '+Comando+' )');

  Resp := FComandoIntegrador.EnviaComando(numeroSessao,'VerificarStatusValidador',Comando);

  Result := TRespostaVerificarStatusValidador.Create;
  Result.AsXMLString := Resp;
end;

function TACBrIntegrador.RespostaFiscal(
  ARespostaFiscal: TRespostaFiscal): TRetornoRespostaFiscal;
var
  Comando, Resp : String;
begin
{$IFNDEF COMPILER23_UP}
  Result := Nil;
{$ENDIF}
  GerarNumeroSessao;

  ARespostaFiscal.Identificador := numeroSessao;
  Comando := ARespostaFiscal.AsXMLString;
  DoLog('RespostaFiscal( '+Comando+' )');

  Resp := FComandoIntegrador.EnviaComando(numeroSessao,'RespostaFiscal',Comando);

  Result := TRetornoRespostaFiscal.Create;
  Result.AsXMLString := Resp;
end;

function TACBrIntegrador.ConsultarNumeroSessaoIntegrador(ANumeroSessao: Integer
  ): String;
begin
  DoLog('ConsultarNumeroSessaoIntegrador( '+IntToStr(ANumeroSessao)+' )');

  Clear;
  NomeComponente := 'ConsultaNumeroSessao';
  NomeMetodo := 'numeroSessao';
  Parametros.Values['numeroSessao'] := IntToStr(ANumeroSessao);
  Result := Enviar(False);
end;

end.

