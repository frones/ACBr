{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Antonio Carlos Junior                           }
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

unit ACBrLibAbecsPinpadBase;

interface

uses
  Classes, SysUtils, TypInfo,
  ACBrLibComum, ACBrLibAbecsPinpadDataModule, ACBrAbecsPinPad, ACBrUtil.FilesIO, ACBrUtil.Strings;

type

  {TACBrLibAbecsPinpad}

  TACBrLibAbecsPinpad = class(TACBrLib)
    private
      FAbecsPinpadDM: TLibAbecsPinpadDM;

    protected
      procedure Inicializar; override;
      procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
      procedure Executar; override;

    public
      constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
      destructor Destroy; override;

      property AbecsPinpadDM: TLibAbecsPinpadDM read FAbecsPinpadDM;

      function Ativar: longint;
      function Desativar: longint;
      function OPN: longint;
      function CLO(const sMensagem: PChar): longint;
      function CLX(const sMensagemOuNomeImagem: PChar): longint;
      function GIX(const PP_DATA: PChar; const sResposta: PChar; var esTamanho: longint): longint;
      function GIN(const GIN_ACQIDX: longint; const sResposta: PChar; var esTamanho: longint): longint;
      function PinPadCapabilities(const sResposta: PChar; var esTamanho: longint): longint;
      function DSP(const sMensagem: PChar): longint;
      function DEX(const sMensagem: PChar): longint;
      function GKY: longint;
      function RMC(const sMensagemRMC: PChar): longint;
      function GCD(aMSGIDX: longint; aTimeOut: longint; const sResposta: PChar; var esTamanho: longint): longint;
      function CEX(VerifyKey: Boolean; VerifyMagnetic: Boolean; VerifyICCInsertion: Boolean; VerifyICCRemoval: Boolean; VerifyCTLSPresence: Boolean; aTimeOut: longint; const sResposta: PChar; var esTamanho: longint): longint;
      function MNU(const sMNUOPT: PChar; sDSPMSG: PChar; aTimeOut: longint; const sResposta: PChar; var esTamanho: longint): longint;
      function LoadMedia(const sCaminhoImagem: PChar; aTipoImagem: longint; const sResposta: PChar; var esTamanho: longint): longint;
      function DSI(const sNomeArquivo: PChar): longint;
      function DMF(const sNomeArquivo: PChar): longint;
  end;

implementation

uses
  ACBrLibConsts, ACBrLibConfig, ACBrLibAbecsPinpadConfig, ACBrLibAbecsPinpadRespostas;

{ TACBrLibAbecsPinpad }

procedure TACBrLibAbecsPinpad.Inicializar;
begin
  inherited Inicializar;

  GravarLog('TACBrLibAbecsPinpad.Inicializar - Feito', logParanoico);
end;

procedure TACBrLibAbecsPinpad.CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = '');
begin
  fpConfig := TLibAbecsPinpadConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibAbecsPinpad.Executar;
begin
  inherited Executar;
  FAbecsPinpadDM.AplicarConfiguracoes;
end;

constructor TACBrLibAbecsPinpad.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);
  FAbecsPinpadDM := TLibAbecsPinpadDM.Create(nil);
  FAbecsPinpadDM.Lib := Self;
end;

destructor TACBrLibAbecsPinpad.Destroy;
begin
  FAbecsPinpadDM.Free;
  inherited Destroy;
end;

function TACBrLibAbecsPinpad.Ativar: longint;
begin
  try
     GravarLog('AbecsPinpad_Ativar', logNormal);

     AbecsPinpadDM.Travar;
     try
        AbecsPinpadDM.ACBrAbecsPinPad1.Enable;
        Result := SetRetorno(ErrOK);
     finally
       AbecsPinpadDM.Destravar;
     end;
  except
    on E: EACBrLibException do
       Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
       Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibAbecsPinpad.Desativar: longint;
begin
  try
     GravarLog('AbecsPinpad_Desativar', logNormal);

     AbecsPinpadDM.Travar;
     try
        AbecsPinpadDM.ACBrAbecsPinPad1.Disable;
        AbecsPinpadDM.ACBrAbecsPinPad1.IsEnabled := False;
        Result := SetRetorno(ErrOK);
     finally
       AbecsPinpadDM.Destravar;
     end;
  except
    on E: EACBrLibException do
       Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
       Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibAbecsPinpad.OPN: longint;
begin
  try
     GravarLog('AbecsPinpad_OPN', logNormal);

     AbecsPinpadDM.Travar;
     try
        AbecsPinpadDM.ACBrAbecsPinPad1.OPN;
        Result := SetRetorno(ErrOK);
     finally
       AbecsPinpadDM.Destravar;
     end;
  except
    on E: EACBrLibException do
       Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
       Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibAbecsPinpad.CLO(const sMensagem: PChar): longint;
var
  Mensagem: String;
begin
  try
     Mensagem := ConverterAnsiParaUTF8(sMensagem);

     if Config.Log.Nivel > logNormal then
        GravarLog('AbecsPinpad_CLO(' + Mensagem + ' )', logCompleto, True)
     else
         GravarLog('AbecsPinpad_CLO', logNormal);

     AbecsPinpadDM.Travar;
     try
        AbecsPinpadDM.ACBrAbecsPinPad1.CLO(Mensagem);
        Result := SetRetorno(ErrOK);
     finally
       AbecsPinpadDM.Destravar;
     end;
  except
    on E: EACBrLibException do
       Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
       Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibAbecsPinpad.CLX(const sMensagemOuNomeImagem: PChar): longint;
var
  MensagemOuNomeImagem: String;
begin
  try
     MensagemOuNomeImagem := ConverterAnsiParaUTF8(sMensagemOuNomeImagem);

     if Config.Log.Nivel > logNormal then
        GravarLog('AbecsPinpad_CLX(' + MensagemOuNomeImagem + ' )', logCompleto, true)
     else
         GravarLog('AbecsPinpad_CLX', logNormal);

     AbecsPinpadDM.Travar;
     try
        AbecsPinpadDM.ACBrAbecsPinPad1.CLX(MensagemOuNomeImagem);
        Result := SetRetorno(ErrOK);
     finally
       AbecsPinpadDM.Destravar;
     end;
  except
    on E: EACBrLibException do
       Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
       Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibAbecsPinpad.GIX(const PP_DATA: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TLibAbecsPinpadRespostaGIX;
  Resposta: AnsiString;
  PPDataArray: Array of Word;
  PPDATA: String;
  i: integer;
  sl: TStringList;
begin
  Try
     PPDATA := ConverterAnsiParaUTF8(PP_DATA);

     if Config.Log.Nivel > logNormal then
        GravarLog('AbecsPinpad_GIX(' + PPDATA + ' )', logCompleto, True)
     else
         GravarLog('AbecsPinpad_GIX', logNormal);

     AbecsPinpadDM.Travar;
     try
        if Trim(PPDATA) = '' then
        begin
          SetLength(PPDataArray, 0);
        end
        else
        begin
        sl := TStringList.Create;
           try
              sl.CommaText := PPDATA;
              SetLength(PPDataArray, sl.Count);
              for i := 0 to sl.Count - 1 do
              begin
              PPDataArray[i] := PP_StrToInt(Trim(sl[i]));
              end;
           finally
             sl.Free;
           end;
        end;

        AbecsPinpadDM.ACBrAbecsPinPad1.GIX(PPDataArray);
        Resp := TLibAbecsPinpadRespostaGIX.Create(Config.TipoResposta, Config.CodResposta);
        try
           Resp.ProcessarGIX(AbecsPinpadDM.ACBrAbecsPinPad1.Response);

           Resposta := Resp.Gerar;
           MoverStringParaPChar(Resposta, sResposta, esTamanho);
           Result := SetRetorno(ErrOK, Resposta);
        finally
          Resp.Free;
        end;
     finally
       AbecsPinpadDM.Destravar;
     end;
  except
    on E: EACBrLibException do
       Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
       Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibAbecsPinpad.GIN(const GIN_ACQIDX: longint; const sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TLibAbecsPinpadRespostaGIN;
  Resposta: AnsiString;
begin
  try
     if Config.Log.Nivel > logNormal then
        GravarLog('AbecsPinpad_GIN(' + IntToStr(GIN_ACQIDX) + ' )', logCompleto, True)
     else
         GravarLog('AbecsPinpad_GIN', logNormal);

     AbecsPinpadDM.Travar;
     try
        AbecsPinpadDM.ACBrAbecsPinPad1.GIN(GIN_ACQIDX);
        Resp := TLibAbecsPinpadRespostaGIN.Create(Config.TipoResposta, Config.CodResposta);
        try
           Resp.ProcessarGIN(AbecsPinpadDM.ACBrAbecsPinPad1.Response);

           Resposta := Resp.Gerar;
           MoverStringParaPChar(Resposta, sResposta, esTamanho);
           Result := SetRetorno(ErrOK, Resposta);
        finally
          Resp.Free;
        end;
     finally
       AbecsPinpadDM.Destravar;
     end;
  except
    on E: EACBrLibException do
       Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
       Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibAbecsPinpad.PinPadCapabilities(const sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TLibAbecsPinpadCapabilitiesResposta;
  Resposta: AnsiString;
begin
  try
     GravarLog('AbecsPinpad_PinPadCapabilities', logNormal);

     AbecsPinpadDM.Travar;
     try
        AbecsPinpadDM.ACBrAbecsPinPad1.PinPadCapabilities;
        Resp := TLibAbecsPinpadCapabilitiesResposta.Create(Config.TipoResposta, Config.CodResposta);
        try
           Resp.Processar(AbecsPinpadDM.ACBrAbecsPinPad1.PinPadCapabilities);

           Resposta := Resp.Gerar;
           MoverStringParaPChar(Resposta, sResposta, esTamanho);
           Result := SetRetorno(ErrOK, Resposta);
        finally;
          Resp.Free;
        end;
     finally
       AbecsPinpadDM.Destravar;
     end;
  except
    on E: EACBrLibException do
       Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
       Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibAbecsPinpad.DSP(const sMensagem: PChar): longint;
var
  Mensagem: String;
begin
  try
     Mensagem := ConverterAnsiParaUTF8(sMensagem);

     if Config.Log.Nivel > logNormal then
        GravarLog('AbecsPinpad_DSP(' + Mensagem + ' )', logCompleto, True)
     else
         GravarLog('AbecsPinpad_DSP', logNormal);

     AbecsPinpadDM.Travar;
     try
        AbecsPinpadDM.ACBrAbecsPinPad1.DSP(Mensagem);
        Result := SetRetorno(ErrOK);
     finally
       AbecsPinpadDM.Destravar;
     end;
  except
    on E: EACBrLibException do
       Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
       Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibAbecsPinpad.DEX(const sMensagem: PChar): longint;
var
  Mensagem: String;
begin
  try
     Mensagem := ConverterAnsiParaUTF8(sMensagem);

     if Config.Log.Nivel > logNormal then
        GravarLog('AbecsPinpad_DEX(' + Mensagem + ' )', logCompleto, True)
     else
         GravarLog('AbecsPinpad_DEX', logNormal);

     AbecsPinpadDM.Travar;
     try
        AbecsPinpadDM.ACBrAbecsPinPad1.DEX(Mensagem);
        Result := SetRetorno(ErrOK);
     finally
       AbecsPinpadDM.Destravar;
     end;
  except
    on E: EACBrLibException do
       Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
       Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibAbecsPinpad.GKY: longint;
var
  i:integer;
  s: String;
begin
  try
     GravarLog('AbecsPinpad_GKY', logNormal);

     AbecsPinpadDM.Travar;
     try
        i := 0;
        s := 'PRESS FUNCTION KEY';
        AbecsPinpadDM.ACBrAbecsPinPad1.DSP(s);

        try
           i := AbecsPinpadDM.ACBrAbecsPinPad1.GKY;
        except
          On E: EACBrAbecsPinPadTimeout do
             Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
          else
          raise;
        end;
        s := Format('Key Number: %d', [i]);
        AbecsPinpadDM.ACBrAbecsPinPad1.DSP(s);
     finally
       AbecsPinpadDM.Destravar;
     end;
  except
    on E: EACBrLibException do
       Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
       Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibAbecsPinpad.RMC(const sMensagemRMC: PChar): longint;
var
  MensagemRMC: String;
begin
  try
     MensagemRMC := ConverterAnsiParaUTF8(sMensagemRMC);

     if Config.Log.Nivel > logNormal then
        GravarLog('AbecsPinpad_RMC(' + MensagemRMC + ' )', logCompleto, True)
     else
         GravarLog('AbecsPinpad_RMC', logNormal);

     AbecsPinpadDM.Travar;
     try
        AbecsPinpadDM.ACBrAbecsPinPad1.RMC(MensagemRMC);
        Result := SetRetorno(ErrOK);
     finally
       AbecsPinpadDM.Destravar;
     end;
  except
    on E: EACBrLibException do
       Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
       Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibAbecsPinpad.GCD(aMSGIDX: longint; aTimeOut: longint; const sResposta: PChar; var esTamanho: longint): longint;
var
  MSGIDX: TACBrAbecsMSGIDX;
  Resp: TLibAbecsPinpadRespostaGCD;
  Resposta: AnsiString;
begin
  try
     if Config.Log.Nivel > logNormal then
        GravarLog('AbecsPinpad_GCD(' + IntToStr(aMSGIDX) + ',' + IntToStr(aTimeOut) + ' )', logCompleto, True)
     else
         GravarLog('AbecsPinpad_GCD', logNormal);

     MSGIDX := TACBrAbecsMSGIDX(aMSGIDX);
     AbecsPinpadDM.Travar;
     try
        AbecsPinpadDM.ACBrAbecsPinPad1.GCD(TACBrAbecsMSGIDX(MSGIDX), aTimeOut);
        Resp := TLibAbecsPinpadRespostaGCD.Create(Config.TipoResposta, Config.CodResposta);
        try
           Resp.ProcessarGCD(AbecsPinpadDM.ACBrAbecsPinPad1.Response);

           Resposta := Resp.Gerar;
           MoverStringParaPChar(Resposta, sResposta, esTamanho);
           Result := SetRetorno(ErrOK, Resposta);
        finally
          Resp.Free;
        end;
     finally
       AbecsPinpadDM.Destravar;
     end;
  except
    on E: EACBrLibException do
       Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
       Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibAbecsPinpad.CEX(VerifyKey: Boolean; VerifyMagnetic: Boolean; VerifyICCInsertion: Boolean; VerifyICCRemoval: Boolean; VerifyCTLSPresence: Boolean; aTimeOut: longint; const sResposta: PChar; var esTamanho: longint): longint;
var
  s: String;
  Resp: TLibAbecsPinpadRespostaCEX;
  Resposta: AnsiString;
begin
  try
     if Config.Log.Nivel > logNormal then
        GravarLog('AbecsPinpad_CEX(' + BoolToStr(VerifyKey) + ',' + BoolToStr(VerifyMagnetic) + ',' + BoolToStr(VerifyICCInsertion) + ',' + BoolToStr(VerifyICCRemoval) + ',' + BoolToStr(VerifyCTLSPresence) + ',' + IntToStr(aTimeOut) + ' )', logCompleto, True)
     else
         GravarLog('AbecsPinpad_CEX', logNormal);

     AbecsPinpadDM.Travar;
     try
        s := '';
        if VerifyKey = True then
        s := s + 'Press Key'+CR;
        if VerifyMagnetic = True then
        s := s + 'Swipe the card'+CR;
        if VerifyICCInsertion = True then
        s := s + 'Insert card'+CR;
        if VerifyICCRemoval = True then
        s := s + 'Remove card'+CR;
        if VerifyCTLSPresence = True then
        s := s + 'Bring card closer'+CR;

        AbecsPinpadDM.ACBrAbecsPinPad1.DEX(s);

        AbecsPinpadDM.ACBrAbecsPinPad1.CEX(VerifyKey, VerifyMagnetic, VerifyICCInsertion, VerifyICCRemoval, VerifyCTLSPresence, aTimeOut, '');
        Resp := TLibAbecsPinpadRespostaCEX.Create(Config.TipoResposta, Config.CodResposta);
        try
           Resp.ProcessarCEX(AbecsPinpadDM.ACBrAbecsPinPad1.Response);

           Resposta := Resp.Gerar;
           MoverStringParaPChar(Resposta, sResposta, esTamanho);
           Result := SetRetorno(ErrOK, Resposta);
        finally
          Resp.Free;
        end;
     finally
       AbecsPinpadDM.Destravar;
     end;
  except
    on E: EACBrLibException do
       Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
       Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibAbecsPinpad.MNU(const sMNUOPT: PChar; sDSPMSG: PChar; aTimeOut: longint; const sResposta: PChar; var esTamanho: longint): longint;
var
  MNUOPT, DSPMSG: String;
  MNUOPTArray: Array of String;
  Resp: TLibAbecsPinpadRespostaMNU;
  Resposta: AnsiString;
  i: Integer;
  sl: TStringList;
begin
  try
     MNUOPT := ConverterAnsiParaUTF8(sMNUOPT);
     DSPMSG := ConverterAnsiParaUTF8(sDSPMSG);

     if Config.Log.Nivel > logNormal then
        GravarLog('AbecsPinpad_MNU(' + MNUOPT + ',' + DSPMSG + ',' + IntToStr(aTimeOut) + ' )', logCompleto, True )
     else
         GravarLog('AbecsPinpad_MNU', logNormal);

     AbecsPinpadDM.Travar;
     try
        if Trim(MNUOPT) = '' then
        begin
          SetLength(MNUOPTArray, 0);
        end
        else
        begin
        sl := TStringList.Create;
           try
              sl.CommaText := MNUOPT;
              SetLength(MNUOPTArray, sl.Count);
              for i := 0 to sl.Count - 1 do
              begin
              MNUOPTArray[i] := Trim(sl[i]);
              end;
           finally
             sl.Free;
           end;
        end;

        AbecsPinpadDM.ACBrAbecsPinPad1.MNU(MNUOPTArray, DSPMSG, aTimeOut);
        Resp := TLibAbecsPinpadRespostaMNU.Create(Config.TipoResposta, Config.CodResposta);
        try
           Resp.ProcessarMNU(AbecsPinpadDM.ACBrAbecsPinPad1.Response);

           Resposta := Resp.Gerar;
           MoverStringParaPChar(Resposta, sResposta, esTamanho);
           Result := SetRetorno(ErrOK, Resposta);
        finally
          Resp.Free;
        end;
     finally
       AbecsPinpadDM.Destravar;
     end;
  except
    on E: EACBrLibException do
       Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
       Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibAbecsPinpad.LoadMedia(const sCaminhoImagem: PChar; aTipoImagem: longint; const sResposta: PChar; var esTamanho: longint): longint;
var
  ext, filename: String;
  AStream: TMemoryStream;
  CaminhoImagem: AnsiString;
  TipoImagem: TACBrAbecsPinPadMediaType;
  Resp: TLibAbecsPinpadRespostaLoadMedia;
  Resposta: AnsiString;
begin
  try
     CaminhoImagem := ConverterAnsiParaUTF8(sCaminhoImagem);

     if Config.Log.Nivel > logNormal then
        GravarLog('AbecsPinpad_MediaLoad(' + CaminhoImagem + ',' + IntToStr(aTipoImagem) + ' )', logCompleto, True)
        else
        GravarLog('AbecsPinpad_MediaLoad', logNormal);

     TipoImagem := TACBrAbecsPinPadMediaType(aTipoImagem);
     AbecsPinpadDM.Travar;
     AStream := TMemoryStream.Create;
     try
        ext := LowerCase(ExtractFileExt(CaminhoImagem));
        filename := StringReplace(ExtractFileName(CaminhoImagem), ext, '',[]);
        filename := AbecsPinpadDM.ACBrAbecsPinPad1.FormatSPE_MFNAME(filename);
        AStream.LoadFromFile(CaminhoImagem);
        AbecsPinpadDM.ACBrAbecsPinPad1.LoadMedia(filename, AStream, TipoImagem);
        AbecsPinpadDM.ACBrAbecsPinPad1.LMF;
        AbecsPinpadDM.ACBrAbecsPinPad1.DSI(filename);
        Resp := TLibAbecsPinpadRespostaLoadMedia.Create(Config.TipoResposta, Config.CodResposta);
        try
           Resp.ProcessarLoadMedia(AbecsPinpadDM.ACBrAbecsPinPad1.Response);

           Resposta := Resp.Gerar;
           MoverStringParaPChar(Resposta, sResposta, esTamanho);
           Result := SetRetorno(ErrOK, Resposta);
        finally
          Resp.Free;
        end;
     finally
       AStream.Free;
       AbecsPinpadDM.Destravar;
     end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibAbecsPinpad.DSI(const sNomeArquivo: PChar): longint;
var
  NomeArquivo: String;
begin
  try
     NomeArquivo := ConverterAnsiParaUTF8(sNomeArquivo);

     if Config.Log.Nivel > logNormal then
        GravarLog('AbecsPinpad_DSI(' + NomeArquivo + ' )', logCompleto, True)
     else
         GravarLog('AbecsPinpad_DSI', logNormal);

     AbecsPinpadDM.Travar;
     try
        AbecsPinpadDM.ACBrAbecsPinPad1.DSI(NomeArquivo);
        Result := SetRetorno(ErrOK);
     finally
       AbecsPinpadDM.Destravar;
     end;
  except
    on E: EACBrLibException do
       Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
       Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibAbecsPinpad.DMF(const sNomeArquivo: PChar): longint;
var
  NomeArquivo: String;
begin
  try
     NomeArquivo := ConverterAnsiParaUTF8(sNomeArquivo);

     if Config.Log.Nivel > logNormal then
        GravarLog('AbecsPinpad_DMF(' + NomeArquivo + ' )', logCompleto, True)
     else
         GravarLog('AbecsPinpad_DMF', logNormal);

     AbecsPinpadDM.Travar;
     try
        AbecsPinpadDM.ACBrAbecsPinPad1.DMF(NomeArquivo);
        Result := SetRetorno(ErrOK);
     finally
       AbecsPinpadDM.Destravar;
     end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

end.

