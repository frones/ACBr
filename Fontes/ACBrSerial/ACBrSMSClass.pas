{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Alexandre Rocha Lima e Marcondes                }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{ Esse arquivo usa a classe  SynaSer   Copyright (c)2001-2003, Lukas Gebauer   }
{  Project : Ararat Synapse     (Found at URL: http://www.ararat.cz/synapse/)  }
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

{$I ACBr.inc}

unit ACBrSMSClass;

interface

uses
  ACBrDevice, Classes, SysUtils, Contnrs, strUtils;

const
  CTRL_Z = #26;
  FALHA_INICIALIZACAO = 'Não foi possível inicializar o envio da mensagem.';
  FALHA_SIMCARD_SINCRONIZADO = 'SimCard não sincronizado com a rede celular.';
  FALHA_NUMERO_TELEFONE = 'Falha ao definir o número de telefone do destinatário.';
  FALHA_ENVIAR_MENSAGEM = 'Não foi possível enviar a mensagem de texto.';
  FALHA_INDICE_MENSAGEM = 'Indice retornado inválido, mensagem não foi enviada.';
  FALHA_LEITURA_MENSAGEM = 'Não foi possível ler as mensagens do SimCard.';
  FALHA_APAGAR_MENSAGEM = 'Não foi possível apagar a mensagem de texto.';
  FALHA_TROCAR_CENTROMENSAGEM = 'Não foi possível trocar o centro de mensagem.';

type
  EACBrSMSException = class(Exception);

  TACBrSMSModelo = (modNenhum, modDaruma, modZTE, modGenerico);
  TACBrSMSSincronismo = (sinErro, sinSincronizado, sinNaoSincronizado, sinBucandoRede);
  TACBrSMSSimCard = (simCard1, simCard2);
  TACBrSMSFiltro = (fltTudo, fltLidas, fltNaoLidas);

  TACBrSMSProgresso = procedure(const AAtual, ATotal: Integer) of object;
  TACBrSMSAntesEnvio = procedure(const ATelefone, AMensagem: string) of object;
  TACBrSMSAposEnvio = procedure(const ATelefone, AMensagem: string; ADataHora : TDateTime) of object;

  TACBrSMSMensagem = class
  private
    FMensagem: String;
    FTelefone: String;
    FDataHora: TDateTime;
    FCodigo: Integer;
  public
    property Codigo : Integer read FCodigo write FCodigo;
    property DataHora : TDateTime read FDataHora write FDataHora;
    property Telefone: String read FTelefone write FTelefone;
    property Mensagem: String read FMensagem write FMensagem;
  end;

  TACBrSMSMensagens = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TACBrSMSMensagem);
    function GetObject (Index: Integer): TACBrSMSMensagem;
    procedure Insert (Index: Integer; Obj: TACBrSMSMensagem);
  public
    procedure LoadFromFile(const APath: String);
    procedure CarregaSMS(const APath : string);
    function Add: TACBrSMSMensagem; overload;
    function Add (Obj: TACBrSMSMensagem): Integer; overload;
    property Objects [Index: Integer]: TACBrSMSMensagem read GetObject write SetObject; default;
  end;

  TACBrSMSClass = class
  private
    fpRecebeConfirmacao: Boolean;
    fpSimCard: TACBrSMSSimCard;
    fpQuebraMensagens: Boolean;
    fpATTimeOut: Integer;
    fpIntervaloEntreMensagens: Integer;
    fpMensagens: TACBrSMSMensagens;
    procedure SetAtivo(const Value: Boolean);
  protected
    fpDevice: TACBrDevice;
    fpAtivo: Boolean;
    fpUltimaResposta: String;
    fpUltimoComando: String;
    fpAtResult: Boolean;
    fpBandejasSimCard: Integer;
  public
    constructor Create(AOwner: TComponent);
    Destructor Destroy; override;

    procedure Ativar; virtual;
    procedure Desativar; virtual;

    procedure EnviarComando(ACmd: String; ATimeOut: Integer = 0);
    
    function EmLinha: Boolean; virtual;
    function IMEI: String; virtual;
    function IMSI: String; virtual;
    function NivelSinal: Double; virtual;
    function Operadora: String; virtual;
    function Fabricante: String; virtual;
    function ModeloModem: String; virtual;
    function Firmware: String; virtual;
    function CentroMensagem: String; virtual;
    function EstadoSincronismo: TACBrSMSSincronismo; virtual;

    procedure TrocarBandeja(const ASimCard: TACBrSMSSimCard); virtual;
    procedure EnviarSMS(const ATelefone, AMensagem: String;
      var AIndice: String); virtual;
    procedure ApagarSMS(const ANumeroMensagem: String); virtual;
    procedure ListarMensagens(const AFiltro: TACBrSMSFiltro;
      const APath: String); virtual;
    procedure TrocaCentroMensagem(const ACentroMensagem : string); virtual;

    property Ativo: Boolean read fpAtivo write SetAtivo;
    property SimCard: TACBrSMSSimCard read fpSimCard write fpSimCard;
    property ATTimeOut: Integer read fpATTimeOut write fpATTimeOut;
    property ATResult: Boolean read fpATResult write fpATResult;
    property IntervaloEntreMensagens: Integer read fpIntervaloEntreMensagens write fpIntervaloEntreMensagens;
    property RecebeConfirmacao: Boolean read fpRecebeConfirmacao write fpRecebeConfirmacao;
    property QuebraMensagens: Boolean read fpQuebraMensagens write fpQuebraMensagens;
    property BandejasSimCard: Integer read fpBandejasSimCard;
    property UltimaResposta: String read fpUltimaResposta write fpUltimaResposta;
    property UltimoComando: String read fpUltimoComando write fpUltimoComando;
    property Mensagens: TACBrSMSMensagens read fpMensagens write fpMensagens;
  end;

implementation

uses
  ACBrSMS, ACBrUtil;

{ TACBrSMSMensagens }

function TACBrSMSMensagens.Add: TACBrSMSMensagem;
begin
  Result := TACBrSMSMensagem.create;
  Add(Result);
end;

function TACBrSMSMensagens.Add(Obj: TACBrSMSMensagem): Integer;
begin
  Result := inherited Add(Obj);
end;

procedure TACBrSMSMensagens.CarregaSMS(const APath: string);
function FormaDataHora(ADataHora : String) : TDateTime;
begin
  Result := StrToDateTimeDef(Copy(ADataHora, 9, 2) + '/' +
                             Copy(ADataHora, 6, 2) + '/' +
                             Copy(ADataHora, 1, 4) +
                             Copy(ADataHora, 11, Length(ADataHora) -6),
                             StrToDate('01/01/1990'));
end;
const
  DelimitadorSMS = 'READ","';
  DelimitadorID = '+CMGL:';  
var
  ListaSMS : TStringList;
  Conteudo : string;
  I, J : Integer;
  FimSMS : Boolean;
begin
  try
    Self.Clear;
    if not FileExists(APath) then
      raise EACBrSMSException.CreateFmt('Arquivo "%s" não encontrado.', [APath]);
    ListaSMS := TStringList.Create;
    ListaSMS.LoadFromFile(APath);
    if ListaSMS.Count = 0 then
      Exit;
    if (Pos('AT+CMGL', ListaSMS[0]) > 0) then
      ListaSMS.Delete(0);
    if (Pos('O', ListaSMS[ListaSMS.Count -1]) > 0) and
      (Length(Trim(ListaSMS[ListaSMS.Count -1])) = 1) then
        ListaSMS.Delete(ListaSMS.Count -1);
    if ListaSMS.Count = 0 then
      Exit;
    if trim(ListaSMS[ListaSMS.Count -1]) = '' then
      ListaSMS.Delete(ListaSMS.Count -1);
    for I := 0 to ListaSMS.Count -1 do
    begin
      Conteudo := ListaSMS[I];
      if Pos(DelimitadorSMS, Conteudo) > 0 then
      begin
        with Self.Add do
        begin
          Codigo := StrToInt64Def(Trim(Copy(Conteudo, Pos(DelimitadorID, Conteudo) +
            Length(DelimitadorID), Pos(',', Conteudo) - (Pos(DelimitadorID, Conteudo) +
            Length(DelimitadorID)))), -1);
          System.Delete(Conteudo, 1, Pos(DelimitadorSMS, Conteudo) + Length(DelimitadorSMS) -1);
          Telefone := Copy(Conteudo, 1, Pos('"', Conteudo) -1);
          Conteudo := StringReplace(Conteudo, Telefone, '', [rfReplaceAll]);
          Conteudo := StringReplace(Conteudo, '"', '', [rfReplaceAll]);
          Conteudo := StringReplace(Conteudo, ',', ' ', [rfReplaceAll]);
          DataHora := FormaDataHora(Trim(Copy(Conteudo, 1, 21)));
          Mensagem := '';
          J := I + 1;
          FimSMS := (J > ListaSMS.Count -1);
          if not FimSMS then
            FimSMS := Pos(DelimitadorSMS, ListaSMS[J]) > 0
          else if Pos(DelimitadorSMS, ListaSMS[I]) = 0 then
            Mensagem := Mensagem + IfThen(Mensagem <> '', sLineBreak) + ListaSMS[I];
          while not FimSMS do
          begin
            Mensagem := Mensagem + IfThen(Mensagem <> '', sLineBreak) + ListaSMS[J];
            Inc(J);
            FimSMS := (J > ListaSMS.Count -1);
            if not FimSMS then
              FimSMS := Pos(DelimitadorSMS, ListaSMS[J]) > 0;
          end;
        end;
      end
    end;
  finally
    FreeAndNil(ListaSMS);
  end;
end;

function TACBrSMSMensagens.GetObject(Index: Integer): TACBrSMSMensagem;
begin
  Result := inherited GetItem(Index) as TACBrSMSMensagem ;
end;

procedure TACBrSMSMensagens.Insert(Index: Integer; Obj: TACBrSMSMensagem);
begin
  inherited Insert(Index, Obj);
end;

procedure TACBrSMSMensagens.LoadFromFile(const APath: String);
var
  F: TStringList;
  R: TStringList;
  I: Integer;
begin
  if not FileExists(APath) then
    raise EACBrSMSException.CreateFmt('Arquivo "%s" não encontrado.', [APath]);

  F := TStringList.Create;
  R := TStringList.Create;
  try
    F.LoadFromFile(APath);

    Self.Clear;
    for I := 0 to F.Count - 1 do
    begin
      R.Text := StringReplace(F.Strings[I], '|', sLineBreak, [rfReplaceAll]);
      if R.Count = 2 then
      begin
        with Self.Add do
        begin
          Telefone := R.Strings[0];
          Mensagem := R.Strings[1];
        end;
      end;
    end;
  finally
    R.Free;
    F.Free;
  end;
end;

procedure TACBrSMSMensagens.SetObject(Index: Integer; Item: TACBrSMSMensagem);
begin
  inherited SetItem (Index, Item) ;
end;

{ TACBrSMSClass }

function TACBrSMSClass.CentroMensagem: String;
begin
   Self.EnviarComando('AT+CSCA?');

  if Self.ATResult then
  begin
    Result := fpUltimaResposta;
    Delete(Result, 1, Pos('"', Result));
    Result := Trim(Copy(Result, 1, Pos('"', Result) - 1));
  end
  else
    Result := EmptyStr;
end;

constructor TACBrSMSClass.Create(AOwner: TComponent);
begin
  fpDevice := (AOwner as TACBrSMS).Device;
  fpDevice.SetDefaultValues;
  fpDevice.TimeOut := 10000;
  fpDevice.Serial.AtTimeout := 10000;

  fpAtivo := False;
  fpSimCard := simCard1;
  fpRecebeConfirmacao := False;
  fpQuebraMensagens := False;
  fpATResult := False;
  fpATTimeout := 10000;
  fpIntervaloEntreMensagens := 0;
  fpBandejasSimCard := 1;
  fpUltimaResposta := EmptyStr;
  fpUltimoComando := String(EmptyStr);
  fpMensagens := TACBrSMSMensagens.Create;
end;

destructor TACBrSMSClass.Destroy;
begin
  if Assigned(fpDevice) then
    fpDevice := nil;

  if Assigned(fpMensagens) then
    FreeAndNil(fpMensagens); 
  
  inherited Destroy;
end;

function TACBrSMSClass.EmLinha: Boolean;
begin
  Self.EnviarComando('AT');
  Result := fpATResult;
end;

function TACBrSMSClass.EstadoSincronismo: TACBrSMSSincronismo;
var
  RetCmd: String;
begin
  Self.EnviarComando('AT+CREG?');

  if Self.ATResult then
  begin
    RetCmd := fpUltimaResposta;
    RetCmd := Trim(Copy(RetCmd, 1, Pos('OK', RetCmd) - 1));
    RetCmd := Trim(Copy(RetCmd, pos(':', RetCmd) + 1, Length(RetCmd)));

    if RetCmd = '0,1' then
      Result := sinSincronizado
    else if RetCmd = '0,2' then
      Result := sinBucandoRede
    else
      Result := sinNaoSincronizado;
  end
  else
    Result := sinErro;
end;

function TACBrSMSClass.Fabricante: String;
begin
  Self.EnviarComando('AT+CGMI');

  if Self.ATResult then
    Result := Trim(Copy(fpUltimaResposta, 1, Pos('OK', fpUltimaResposta) - 1))
  else
    Result := '';
end;

function TACBrSMSClass.IMEI: String;
begin
  Self.EnviarComando('AT+CGSN');

  if Self.ATResult then
    Result := Trim(Copy(fpUltimaResposta, 1, Pos('OK', fpUltimaResposta) - 1))
  else
    Result := EmptyStr;
end;

function TACBrSMSClass.IMSI: String;
begin
  Self.EnviarComando('AT+CIMI');

  if Self.ATResult then
    Result := Trim(Copy(fpUltimaResposta, 1, Pos('OK', fpUltimaResposta) - 1))
  else
    Result := EmptyStr;
end;

procedure TACBrSMSClass.ListarMensagens(const AFiltro: TACBrSMSFiltro;
  const APath: String);
var
  cmd: String;
  Retorno: String;
  I: Integer;
begin
  case AFiltro of
    fltTudo:     cmd := 'AT+CMGL="ALL"';
    fltLidas:    cmd := 'AT+CMGL="REC READ"';
    fltNaoLidas: cmd := 'AT+CMGL="REC UNREAD"';
  end;
  Self.EnviarComando(cmd);
  if Self.ATResult then
  begin
    Retorno := EmptyStr;
    for I := 0 to Length(fpUltimaResposta) - 1 do
    begin
      if not(fpUltimaResposta[I] in [#0, #5, #$18, #$C]) then
        Retorno := Retorno + fpUltimaResposta[I];
    end;
    fpUltimaResposta := Trim(Retorno);
    WriteToTXT(AnsiString(APath), AnsiString(fpUltimaResposta), False, True);
  end;   
end;

function TACBrSMSClass.ModeloModem: String;
begin
  Self.EnviarComando('AT+CGMM');

  if Self.ATResult then
    Result := Trim(Copy(fpUltimaResposta, 1, Pos('OK', fpUltimaResposta) - 1))
  else
    Result := EmptyStr;
end;

function TACBrSMSClass.NivelSinal: Double;
var
  RetCmd: String;
begin
  Self.EnviarComando('AT+CSQ');

  if Self.ATResult then
  begin
    RetCmd := AnsiUpperCase(fpUltimaResposta);
    RetCmd := Trim(Copy(RetCmd, 1, Pos('OK', RetCmd) - 1));
    RetCmd := Trim(Copy(RetCmd, pos(':', RetCmd) + 1, Length(RetCmd)));

    Result := StrToFloatDef(String(RetCmd), 0.00);
  end
  else
    Result := 0;
end;

function TACBrSMSClass.Operadora: String;
begin
  Self.EnviarComando('AT+COPS?');

  if Self.ATResult then
  begin
    Result := Trim(Copy(fpUltimaResposta, 1, Pos('OK', fpUltimaResposta) - 1));
    Result := Copy(Result, Pos('"', Result) + 1, Length(Result));
    Result := Copy(Result, 1, Pos('"', Result) - 1);
  end
  else
    Result := EmptyStr;
end;

function TACBrSMSClass.Firmware: String;
begin
  Self.EnviarComando('AT+CGMR');

  if Self.ATResult then
    Result := Trim(Copy(fpUltimaResposta, 1, Pos('OK', fpUltimaResposta) - 1))
  else
    Result := EmptyStr;
end;

procedure TACBrSMSClass.SetAtivo(const Value: Boolean);
begin
  if Value then
    Ativar
  else
    Desativar;
end;

procedure TACBrSMSClass.EnviarComando(ACmd: String; ATimeOut: Integer);
var
  sRet: String;
begin
  if ATimeOut = 0 then
    ATimeOut := fpATTimeOut;

  fpUltimaResposta := '';
  fpUltimoComando := ACmd;
  fpAtResult := False;

  fpDevice.Serial.Purge;
  fpDevice.Serial.SendString(AnsiString(ACmd + sLineBreak));

  repeat
    Sleep(100);
    sRet := String(fpDevice.Serial.RecvPacket(ATimeOut));

    if sRet <> ACmd then
      fpUltimaResposta := fpUltimaResposta + sRet;

    if (Pos('OK', sRet) > 0) or
       (Pos('>', sRet) > 0) or
       (Pos('CONNECT', String(sRet)) = 1) then
    begin
      fpAtResult := True;
      break;
    end;

    if (Pos('ERROR', sRet) > 0) or
       (Pos('NO CARRIER', sRet) > 0) or
       (Pos('BUSY', sRet) > 0) or
       (Pos('NO DIALTONE', sRet) > 0) then
    begin
      break;
    end;

  until fpDevice.Serial.LastError <> 0;

  fpUltimaResposta := Trim(fpUltimaResposta);
end;

procedure TACBrSMSClass.Ativar;
begin
  if fpAtivo then
    exit;

  if fpDevice.Porta <> '' then
    fpDevice.Ativar;

  fpAtivo := True;
end;

procedure TACBrSMSClass.Desativar;
begin
  if not fpAtivo then
    exit;

  if fpDevice.Porta <> '' then
     fpDevice.Desativar;

  fpAtivo := False;
end;

procedure TACBrSMSClass.EnviarSMS(const ATelefone, AMensagem: String;
  var AIndice: String);
var
  Cmd: String;
  Ret: String;
begin
  // aguardar o tempo setado antes de enviar a mensagem
  if Self.IntervaloEntreMensagens > 0 then
    Sleep(Self.IntervaloEntreMensagens);

  // definir o modo de envio ***************************************************
  Cmd := 'AT+CMGF=1';
  Self.EnviarComando(Cmd);
  if not Self.ATResult then
    raise EACBrSMSException.Create(FALHA_INICIALIZACAO + sLineBreak + fpUltimaResposta);


  // definir o número de telefone do destinatário ******************************
  Cmd := 'AT+CMGS="' + ATelefone + '"';
  Self.EnviarComando(Cmd);
  if not Self.ATResult then
    raise EACBrSMSException.Create(FALHA_NUMERO_TELEFONE + sLineBreak + fpUltimaResposta);


  // Enviar a mensagem *********************************************************
  Cmd := Trim(AMensagem) + CTRL_Z;
  Self.EnviarComando(Cmd);
  if not Self.ATResult then
    raise EACBrSMSException.Create(FALHA_ENVIAR_MENSAGEM + sLineBreak + fpUltimaResposta);


  // verificar se foi retornado indice da mensagem *****************************
  Ret := fpUltimaResposta;
  if Pos(':', Ret) >= 0 then
  begin
    // separar o indice da mensagem
    Ret := Trim(Copy(Ret, Pos('CMGS:', Ret) + 5, Length(Ret)));
    Ret := Trim(Copy(Ret, 1, Pos('OK', Ret) - 1));

    AIndice := IntToStr(StrToIntDef(Trim(Ret), -1));
    if AIndice = '-1' then
      raise EACBrSMSException.Create(FALHA_INDICE_MENSAGEM + sLineBreak + fpUltimaResposta);
  end
  else
    AIndice := '-1';
end;

procedure TACBrSMSClass.TrocaCentroMensagem(const ACentroMensagem: string);
var
  Cmd: String;
begin
  Cmd := 'AT+CSCA="'+ACentroMensagem+'",145';
  Self.EnviarComando(Cmd);
  if not Self.ATResult then
    raise EACBrSMSException.Create(FALHA_TROCAR_CENTROMENSAGEM + sLineBreak + fpUltimaResposta);
end;

procedure TACBrSMSClass.TrocarBandeja(const ASimCard: TACBrSMSSimCard);
begin
  raise EACBrSMSException.Create('Dispositivo não possui suporte para a troca de bandejas SimCard.');
end;

procedure TACBrSMSClass.ApagarSMS(const ANumeroMensagem: String);
var
  Cmd: String;
begin
  Cmd := 'AT+CMGD=' + ANumeroMensagem;
  Self.EnviarComando(Cmd);
  if not Self.ATResult then
    raise EACBrSMSException.Create(FALHA_APAGAR_MENSAGEM + sLineBreak + fpUltimaResposta);
end;

end.
