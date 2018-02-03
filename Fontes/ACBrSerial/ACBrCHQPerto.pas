{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}
{******************************************************************************
 |* Agradecimentos à:
 |* Miguel Silva  -  Perto S/A - Teste deste Modulo
 ******************************************************************************}

{******************************************************************************
 |* Historico
 |*
 |* 24/08/2004: Daniel Simoes de Almeida
 |*  - Primeira Versao ACBrCHQPerto
 |* 07/07/2009: Marcelo Correia Pinheiro
 |*  - Corrigido bug na comuicação com a impressora e definição de timeouts
 |*    para comandos enviados à impressora.
 ******************************************************************************}

{$I ACBr.inc}

unit ACBrCHQPerto;

interface

uses
  Classes,
  ACBrCHQClass;

type

  TPreenchimentoCheque = (
    tpcPreenchimentoSimples,                             // Somente preenchimento
    tpcPreenchimentoChancela,                            // Preenchimento e chancela
    tpcPreenchimentoLeituraCMC7,                         // Preenchimento e leitura de caracteres magnetizáveis CMC7
    tpcPreenchimentoChancelaLeituraCMC7,                 // Preenchimento, chancela e leitura
    tpcPreenchimentoSimplesComAno4Digitos,               // Preenchimento com ano de 4 dígitos
    tpcPreenchimentoChancelaComAno4Digitos,              // Preenchimento, chancela e ano com 4 dígitos
    tpcPreenchimentoLeituraCMC7ComAno4Digitos,           // Preenchimento, leitura e ano com 4 dígitos
    tpcPreenchimentoChancelaLeituraCMC7ComAno4Digitos,   // Preenchimento, chancela, leitura e ano com 4 dígitos
    tpcPreenchimentoSimplesCruzamento,                   // Preenchimento e cruzamento
    tpcPreenchimentoCruzamentoChancela,                  // Preenchimento, cruzamento e chancela
    tpcPreenchimentoCruzamentoLeituraCMC7,               // Preenchimento, cruzamento e leitura
    tpcPreenchimentoCruzamentoChancelaLeituraCMC7,       // Preenchimento, cruzamento, chancela e leitura
    tpcPreenchimentoCruzamentoComAno4Digitos,            // Preenchimento, cruzamento e ano com 4 dígitos
    tpcPreenchimentoCruzamentoChancelaComAno4Digitos,    // Preenchimento, cruzamento, chancela e ano com 4 dígitos
    tpcPreenchimentoCruzamentoLeituraCMC7ComAno4Digitos, // Preenchimento, cruzamento, leitura e ano com 4 dígitos
    tpcPreenchimentoCruzamentoChancelaLeituraCMC7ComAno4Digitos // Preenchimento, cruzamento, chancela, leitura e ano com 4 dígitos
     );

  TACBrCHQPerto = class(TACBrCHQClass)
  private
    fsAguardandoResposta: Boolean;
    fsBancoLido: string;
    fsContaLida: string;
    fsAgenciaLida: string;
    fsChequeLido: string;
    fsCompLida: string;
    fsPreenchimentoCheque: TPreenchimentoCheque;

  protected
    procedure SetBomPara(const Value: TDateTime); override;

  public
    constructor Create(AOwner: TComponent);

    procedure Ativar; override;

    function EnviaComando(cmd: string; SecTimeOut: Integer = 2): string;
    function PreparaCmd(cmd: string): string;
    procedure VerificaErro(Err: string);

    property AguardandoResposta: Boolean read fsAguardandoResposta;

    property PreenchimentoCheque: TPreenchimentoCheque read fsPreenchimentoCheque write fsPreenchimentoCheque default tpcPreenchimentoCruzamentoChancela;
    property BancoLido: string read fsBancoLido;
    property AgenciaLida: string read fsAgenciaLida;
    property ContaLida: string read fsContaLida;
    property ChequeLido: string read fsChequeLido;
    property CompLida: string read fsCompLida;

    procedure ImprimirCheque; override;
    procedure ImprimirVerso(AStringList: TStrings); override;
    procedure TravarCheque; override;
    procedure DestravarCheque; override;

  end;

function PreenchimentoChequeToStr(const AValue: TPreenchimentoCheque): string;

implementation

uses
  SysUtils, Math,
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5{$ENDIF},
  ACBrDevice, ACBrConsts, ACBrUtil;

function PreenchimentoChequeToStr(const AValue: TPreenchimentoCheque): string;
var
  AInt: Integer;
begin
  AInt := Integer(AValue);
  if AInt > 9 then
    Result := chr(55+AInt)
  else
    Result := IntToStr(AInt);
end;

{ TACBrCHQPerto }

constructor TACBrCHQPerto.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fpModeloStr        := 'Perto';
  fpDevice.HandShake := hsDTR_DSR;

  fsAguardandoResposta := false;
  fsBancoLido          := '';
  fsContaLida          := '';
  fsAgenciaLida        := '';
  fsChequeLido         := '';
  fsCompLida           := '';
end;

procedure TACBrCHQPerto.Ativar;
begin
  if not fpDevice.IsSerialPort then
    raise Exception.Create(ACBrStr('Impressora de Cheques ' + fpModeloStr + ' requer' + #10 +
           'Porta Serial (COMn)'));

  fsAguardandoResposta := false;
  fpDevice.HandShake   := hsDTR_DSR;

  inherited Ativar; { Abre porta serial }
end;

procedure TACBrCHQPerto.TravarCheque;
var
  Resp: string;
begin
  fsBancoLido   := '';
  fsContaLida   := '';
  fsAgenciaLida := '';
  fsChequeLido  := '';
  fsCompLida    := '';

  Resp := EnviaComando('=', Max(fpDevice.TimeOut, 30)); { Ler e Alinhar Cheque }
  VerificaErro(Resp);

  fsBancoLido   := copy(Resp, 4, 3);
  fsAgenciaLida := copy(Resp, 7, 4);
  fsContaLida   := copy(Resp, 11, 10);
  fsChequeLido  := copy(Resp, 21, 6);
  fsCompLida    := copy(Resp, 27, 3);
  fpCMC7        := Resp;
end;

procedure TACBrCHQPerto.DestravarCheque;
begin
  VerificaErro(EnviaComando('>', Max(fpDevice.TimeOut, 30))) { Expulsar Cheque }
end;

procedure TACBrCHQPerto.ImprimirCheque;
var
  ValStr, DataStr: string;
begin
  TravarCheque;

  { Favorecido }
  VerificaErro(EnviaComando('%' + Trim(UpperCase(fpFavorecido))));

  { Cidade }
  VerificaErro(EnviaComando('#' + Trim(UpperCase(fpCidade))));

  { Data }
  DataStr := FormatDateTime('ddmmyy', fpData);
  VerificaErro(EnviaComando('!' + DataStr));

  { Comanda Preenchimento }
  ValStr := IntToStrZero(Round(fpValor * 100), 12);
  VerificaErro(EnviaComando(';' + PreenchimentoChequeToStr(fsPreenchimentoCheque) + ValStr + fpBanco, Max(fpDevice.TimeOut, 30)));
end;

procedure TACBrCHQPerto.ImprimirVerso(AStringList: TStrings);
var
  Texto, Texto1, Texto2: string;
begin
  if AStringList.Text = '' then
    exit;

  Texto1 := '';
  Texto2 := '';
  if AStringList.Count > 2 then
  begin
    Texto  := StringReplace(AStringList.Text, #13 + #10, ' / ', [rfReplaceAll]);
    Texto1 := copy(Texto, 01, 60);
    Texto2 := copy(Texto, 61, 60);
  end
  else
  begin
    if AStringList.Count > 0 then
      Texto1 := AStringList[0];
    if AStringList.Count > 1 then
      Texto2 := AStringList[1];
  end;

  Texto1 := PadRight(UpperCase(TiraAcentos(Texto1)), 60);
  Texto2 := PadRight(UpperCase(TiraAcentos(Texto2)), 60);

  VerificaErro(EnviaComando('"' + Texto1 + #255 + Texto2 + #255, Max(fpDevice.TimeOut, 30)));

end;

function TACBrCHQPerto.EnviaComando(cmd: string; SecTimeOut: Integer): string;
var
  ACK: Byte;
  wTempoLimite: TDateTime;
begin

  Result     := '';
  SecTimeOut := SecTimeOut * 1000;

  fpDevice.Serial.DeadlockTimeout := SecTimeOut; { Ajusta Timeout }

  try
    if not fpDevice.EmLinha(3) then { Impressora está em-linha ? }
      raise Exception.Create(ACBrStr('A impressora de Cheques ' + fpModeloStr +
             ' não está pronta.'));

    { Para evita chamadas recursivas, enquanto já está esperando uma resposta }
    fsAguardandoResposta := true;
    try
      { Codificando CMD de acordo com o protocolo da PertoCheck }
      cmd              := PreparaCmd(cmd);
      fpComandoEnviado := cmd;

      repeat
        try
          fpDevice.Limpar;
          fpDevice.EnviaString(cmd); { Eviando o comando }

          { põe pra dormir para atualizar o buffer da porta serial... }
          Sleep(200);
        except
          raise Exception.create(ACBrStr('Erro ao enviar comandos para a PertoCheck'));
        end;

        try
          ACK := fpDevice.LeByte(SecTimeOut);
        except
          raise Exception.create(ACBrStr('PertoCheck não responde'));
        end;

        if ACK = 21 then
          raise Exception.create(ACBrStr('PertoCheck não reconheceu o comando'));
      until ACK = 6;

      { Le conteudo da porta }
      { Calcula Tempo Limite. Espera resposta até Tempo Limite. Se a resposta
        for Lida antes, já encerra. Se nao chegar até TempoLimite, gera erro.}
      fpRespostaComando := '';
      wTempoLimite      := IncMilliSecond(now, SecTimeOut);

      repeat
        try
          fpRespostaComando := fpRespostaComando + { Le conteudo da porta }
             fpDevice.LeString(SecTimeOut);
        except
          { Exceçao silenciosa }
        end;

        if now > wTempoLimite then { TimeOut }
          raise Exception.create(ACBrStr('Impressora PertoCheck não está respondendo'));
      until (copy(fpRespostaComando, length(fpRespostaComando) - 1, 1) = #3);

      { Separando o Retorno... Tirando STX, cmd, ETX, BCC }
      fpRespostaComando := copy(fpRespostaComando, 3, length(fpRespostaComando) - 4);
      Result            := fpRespostaComando;
    finally
      fsAguardandoResposta := false;
    end;
  except
    raise;
  end;
end;

function TACBrCHQPerto.PreparaCmd(cmd: string): string;
var
  A: Integer;
  BCC: Byte;
begin

  result := '';
  if cmd = '' then
    exit;

  cmd := STX + cmd + ETX; { STX + COMANDO + DADOS + ETX }

  { Calculando BCC }
  BCC   := ord(cmd[1]) xor ord(cmd[2]);
  for A := 3 to Length(cmd) do
    BCC := BCC xor ord(cmd[A]);

  result := cmd + chr(BCC);
end;

procedure TACBrCHQPerto.VerificaErro(Err: string);
var
  MsgErro: string;
begin
  Err := IntToStrZero(StrToIntDef(copy(Err, 1, 3), 0), 3);

  if Err = '000' then
    exit;

  MsgErro := '';
  if Err = '001' then
    MsgErro := 'Mensagem com dados inválidos.';
  if Err = '002' then
    MsgErro := 'Tamanho de mensagem inválido.';
  if Err = '005' then
    MsgErro := 'Leitura dos caracteres magnéticos inválida.';
  if Err = '006' then
    MsgErro := 'Problemas no acionamento do motor 1.';
  if Err = '008' then
    MsgErro := 'Problemas no acionamento do motor 2.';
  if Err = '009' then
    MsgErro := 'Banco diferente do solicitado.';
  if Err = '011' then
    MsgErro := 'Sensor 1 obstruído.';
  if Err = '012' then
    MsgErro := 'Sensor 2 obstruído.';
  if Err = '013' then
    MsgErro := 'Sensor 4 obstruído.';
  if Err = '014' then
    MsgErro := 'Erro no posicionamento da cabeça de impressão (relativo a S4).';
  if Err = '016' then
    MsgErro := 'Dígito verificador do cheque não confere.';
  if Err = '017' then
    MsgErro := 'Ausência de caracteres magnéticos ou cheque na posição errada.';
  if Err = '018' then
    MsgErro := 'Tempo esgotado.';
  if Err = '019' then
    MsgErro := 'Documento mal inserido.';
  if Err = '020' then
    MsgErro := 'Cheque preso durante o alinhamento (S1 e S2 desobstruídos).';
  if Err = '021' then
    MsgErro := 'Cheque preso durante o alinhamento (S1 obstruído e S2 desobstruído).';
  if Err = '022' then
    MsgErro := 'Cheque preso durante o alinhamento (S1 desobstruído e S2 obstruído).';
  if Err = '023' then
    MsgErro := 'Cheque preso durante o alinhamento (S1 e S2 obstruídos).';
  if Err = '024' then
    MsgErro := '	Cheque preso durante o preenchimento (S1 e S2 desobstruídos).';
  if Err = '025' then
    MsgErro := 'Cheque preso durante o preenchimento (S1 obstruído e S2 desobstruído).';
  if Err = '026' then
    MsgErro := 'Cheque preso durante o preenchimento (S1 desobstruído e S2 obstruído).';
  if Err = '027' then
    MsgErro := 'heque preso durante o preenchimento (S1 e S2 obstruídos).';
  if Err = '028' then
    MsgErro := 'Caractere inexistente.';
  if Err = '030' then
    MsgErro := 'Não há cheques na memória.';
  if Err = '031' then
    MsgErro := 'Lista negra interna cheia';
  if Err = '042' then
    MsgErro := 'Cheque ausente.';
  if Err = '050' then
    MsgErro := 'Erro de transmissão.';
  if Err = '051' then
    MsgErro := 'Erro de transmissão: Impressora offline, desconectada ou ocupada.';
  if Err = '060' then
    MsgErro := 'Cheque na lista negra.';
  if Err = '073' then
    MsgErro := 'Cheque não encontrado na lista negra.';
  if Err = '074' then
    MsgErro := 'Comando cancelado.';
  if Err = '084' then
    MsgErro := 'Arquivo de layout´s cheio';
  if Err = '085' then
    MsgErro := 'Layout inexistente na memória.';
  if Err = '091' then
    MsgErro := 'Leitura de cartão inválida.';
  if Err = '097' then
    MsgErro := 'Cheque na posição errada.';
  if Err = '255' then
    MsgErro := 'Comando inexistente.';

  MsgErro := 'PertoCheck retorno erro: ' + Err + #10 + ' ' + MsgErro;

  raise Exception.Create(ACBrStr(MsgErro));

end;

procedure TACBrCHQPerto.SetBomPara(const Value: TDateTime);
begin
  inherited SetBomPara(Value);
  VerificaErro(EnviaComando('+BOM PARA: ' + FormatDateTime('dd/mm/yy', fpBomPara)));
end;

end.
