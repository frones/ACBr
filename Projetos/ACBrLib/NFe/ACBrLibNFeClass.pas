{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo: Rafael Teno Dias                                }

{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }

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
{ http://www.opensource.org/licenses/gpl-license.php                           }

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{        Rua Cel.Aureliano de Camargo, 973 - Tatuí - SP - 18270-170            }

{******************************************************************************}

{$I ACBr.inc}

unit ACBrLibNFeClass;

interface

uses
  Classes, SysUtils,
  ACBrLibComum, ACBrLibNFeDataModule;

type

  { TACBrLibNFe }

  TACBrLibNFe = class( TACBrLib )
  private
    FNFeDM: TLibNFeDM;

  protected
    procedure Inicializar; override;
    procedure CriarConfiguracao(ArqConfig: String = ''; ChaveCrypt: AnsiString = ''); override;
    procedure Executar; override;
    procedure Finalizar; override;
  public
    property NFeDM: TLibNFeDM read FNFeDM;
  end;

{%region Declaração da funções}

{%region NFe}
function NFE_CarregarXMLNFe(const eArquivoOuXML: PChar): Integer;
  {$IfDef STDCALL}stdcall{$Else}cdecl{$EndIf};

function NFE_CarregarININFe(const eArquivoOuINI: PChar): Integer;
  {$IfDef STDCALL}stdcall{$Else}cdecl{$EndIf};

function NFE_LimparListaNFEs: Integer;
  {$IfDef STDCALL}stdcall{$Else}cdecl{$EndIf};
{%endregion}


{%region Servicos}
function NFE_StatusServico(const Buffer: PChar; const BufferLen: Integer): Integer;
  {$IfDef STDCALL}stdcall{$Else}cdecl{$EndIf};
{%endregion}

{%region Impressão}
function NFE_Print_PDF_NFe(const ArqXml: PChar): Integer;
  {$IfDef STDCALL}stdcall{$Else}cdecl{$EndIf};
{%endregion}

{%endregion}

implementation

uses
  ACBrLibConsts, ACBrLibNFeConsts, ACBrLibConfig, ACBrLibNFeConfig,
  pcnConversao, pcnAuxiliar,
  blcksock,
  ACBrMail, ACBrUtil;

{ TACBrLibNFe }

procedure TACBrLibNFe.Inicializar;
begin
  inherited Inicializar;

  fpNome := CLibNFeNome;
  fpVersao := CLibNFeVersao;
  FNFeDM := TLibNFeDM.Create(Nil);
  pLib.GravarLog('TACBrLibNFe.Inicializar - Feito', logParanoico);
end;

procedure TACBrLibNFe.CriarConfiguracao(ArqConfig: String; ChaveCrypt: AnsiString);
begin
  fpConfig := TLibNFeConfig.Create(Self, ArqConfig, ChaveCrypt);
  pLib.GravarLog('TACBrLibNFe.CriarConfiguracao - Feito', logParanoico);
end;

procedure TACBrLibNFe.Executar;
begin
  inherited Executar;
  FNFeDM.AplicarConfiguracoes;
end;

procedure TACBrLibNFe.Finalizar;
begin
  FNFeDM.Free;
  inherited Finalizar;
end;


{%region NFe}

function NFE_CarregarXMLNFe(const eArquivoOuXML: PChar): Integer;
  {$IfDef STDCALL}stdcall{$Else}cdecl{$EndIf};
var
  Ok: Boolean;
  ArquivoOuXml: String;
begin
  if not VerificarInicializacao then
  begin
    Result := ErrLibNaoInicializada;
    Exit;
  end;

  Result := 0;
  Ok := False;
  ArquivoOuXml := String(eArquivoOuXML);

  if pLib.Config.Log.Nivel > logNormal then
    pLib.GravarLog('NFE_CarregarXMLNFe(' +ArquivoOuXml+' )', logCompleto, True)
  else
    pLib.GravarLog('NFE_CarregarXMLNFe', logNormal);

  with TACBrLibNFe(pLib) do
  begin
    NFeDM.Travar;
    try
      try
        if StringEhArquivo(ArquivoOuXml) then
        begin
          if not FileExists(ArquivoOuXml) then
          begin
            Result := pLib.SetRetorno( ErrArquivoNaoExiste, Format(SErrArquivoNaoExiste, [ArquivoOuXml] ));
            Exit;
          end;

          Ok := NFeDM.ACBrNFe.NotasFiscais.LoadFromFile(ArquivoOuXml)
        end
        else
          Ok := NFeDM.ACBrNFe.NotasFiscais.LoadFromString(ArquivoOuXml);

        if Ok then
          Result := NFeDM.ACBrNFe.NotasFiscais.Count
        else
          Result := pLib.SetRetorno( 0, SErrNFeNenhumaNFeCarregada );
      except
        on E: Exception do
        begin
          Result := pLib.SetRetorno( ErrExecutandoMetodo, E.Message );
        end
      end;
    finally
      NFeDM.Destravar;
    end;
  end;
end;

function NFE_CarregarININFe(const eArquivoOuINI: PChar): Integer;
  {$IfDef STDCALL}stdcall{$Else}cdecl{$EndIf};
var
  ArquivoOuINI: String;
begin
  if not VerificarInicializacao then
  begin
    Result := ErrLibNaoInicializada;
    Exit;
  end;

  Result := 0;
  ArquivoOuINI := String(eArquivoOuINI);

  if pLib.Config.Log.Nivel > logNormal then
    pLib.GravarLog('NFE_CarregarININFe(' +ArquivoOuINI+' )', logCompleto, True)
  else
    pLib.GravarLog('NFE_CarregarININFe', logNormal);

  if StringEhArquivo(ArquivoOuINI) and (not FileExists(ArquivoOuINI)) then
  begin
    Result := pLib.SetRetorno( ErrArquivoNaoExiste, Format(SErrArquivoNaoExiste, [ArquivoOuINI] ));
    Exit;
  end;

  with TACBrLibNFe(pLib) do
  begin
    NFeDM.Travar;
    try
      try
        if NFeDM.ACBrNFe.NotasFiscais.LoadFromIni(ArquivoOuINI) then
          Result := NFeDM.ACBrNFe.NotasFiscais.Count
        else
          Result := pLib.SetRetorno( 0, SErrNFeNenhumaNFeCarregada );
      except
        on E: Exception do
        begin
          Result := pLib.SetRetorno( ErrExecutandoMetodo, E.Message );
        end
      end;
    finally
      NFeDM.Destravar;
    end;
  end;
end;

function NFE_LimparListaNFEs: Integer;
  {$IfDef STDCALL}stdcall{$Else}cdecl{$EndIf};
begin
  if not VerificarInicializacao then
  begin
    Result := ErrLibNaoInicializada;
    Exit;
  end;

  pLib.GravarLog('NFE_LimparListaNFEs', logNormal);

  with TACBrLibNFe(pLib) do
  begin
    NFeDM.Travar;
    try
      try
        NFeDM.ACBrNFe.NotasFiscais.Clear;
        Result := NFeDM.ACBrNFe.NotasFiscais.Count;
      except
        on E: Exception do
        begin
          Result := pLib.SetRetorno( ErrExecutandoMetodo, E.Message );
        end
      end;
    finally
      NFeDM.Destravar;
    end;
  end;
end;

{%endregion}

{%region Servicos}

function NFE_StatusServico(const Buffer: PChar; const BufferLen: Integer
  ): Integer;
  {$IfDef STDCALL}stdcall{$Else}cdecl{$EndIf};
var
  Resposta: String;
begin
  if (pLibNFeDM = nil) then
  begin
    Result := -2;
    Exit;
  end;

  try

    pLibNFeDM.Lock.Acquire;

    try
      with pLibNFeDM.ACBrNFe do
      begin
        WebServices.StatusServico.Executar;
        Resposta := WebServices.StatusServico.Msg +
          '[STATUS]' + sLineBreak +
          'Versao=' + WebServices.StatusServico.verAplic + sLineBreak +
          'TpAmb=' + TpAmbToStr(WebServices.StatusServico.TpAmb) + sLineBreak +
          'VerAplic=' + WebServices.StatusServico.VerAplic + sLineBreak +
          'CStat=' + IntToStr(WebServices.StatusServico.CStat) + sLineBreak +
          'XMotivo=' + WebServices.StatusServico.XMotivo + sLineBreak +
          'CUF=' + IntToStr(WebServices.StatusServico.CUF) + sLineBreak +
          'DhRecbto=' + DateTimeToStr(WebServices.StatusServico.DhRecbto) + sLineBreak +
          'TMed=' + IntToStr(WebServices.StatusServico.TMed) + sLineBreak +
          'DhRetorno=' + DateTimeToStr(WebServices.StatusServico.DhRetorno) + sLineBreak +
          'XObs=' + WebServices.StatusServico.XObs + sLineBreak;
      end;

      StrPLCopy(Buffer, Resposta, BufferLen);
      Result := 0;
    except
      on E: Exception do
      begin
        pLibRetorno.Mensagem := E.Message;
        Result := -1;
      end
    end;

  finally
    pLibNFeDM.Lock.Release;
  end;
end;

{%endregion}

{%region Impressão}

function NFE_Print_PDF_NFe(const ArqXml: PChar): Integer;
  {$IfDef STDCALL}stdcall{$Else}cdecl{$EndIf};
begin

  if (pLibNFeDM = nil) then
  begin
    Result := -2;
    Exit;
  end;

  try

    pLibNFeDM.Lock.Acquire;

    try
      pLibNFeDM.ACBrNFe.NotasFiscais.Clear;
      pLibNFeDM.ACBrNFe.NotasFiscais.LoadFromFile(ansistring(ArqXml));
      pLibNFeDM.ACBrNFe.DANFE := pLibNFeDM.ACBrNFeDANFeRL;
      pLibNFeDM.ACBrNFe.DANFE.ImprimirDANFEPDF;
      pLibNFeDM.ACBrNFe.DANFE := nil;
      Result := 0;
    except
      on E: Exception do
      begin
        pLibRetorno.Mensagem := E.Message;
        Result := -1;
      end
    end;

  finally
    pLibNFeDM.Lock.Release;
  end;
end;

{%endregion}

exports
  // Servicos
  NFE_StatusServico,

  // Arquivos
  NFE_CarregarXMLNFe,
  NFE_CarregarININFe,

  // Impressão
  NFE_Print_PDF_NFe;

end.




function NFE_Config_SetEmail(config: Integer; const Value: PChar): Integer;
  {$IfDef STDCALL}stdcall;{$Else}cdecl;{$EndIf} export;

function NFE_Config_GetEmail(config: Integer; const Buffer: PChar; const BufferLen: Integer): Integer;
  {$IfDef STDCALL}stdcall;{$Else}cdecl;{$EndIf} export;

function NFE_Config_GetWebService(config: Integer; const Buffer: PChar; const BufferLen: Integer): Integer;
  {$IfDef STDCALL}stdcall;{$Else}cdecl;{$EndIf} export;

function NFE_Config_SetWebService(config: Integer; const Value: PChar): Integer;
  {$IfDef STDCALL}stdcall;{$Else}cdecl;{$EndIf} export;

function NFE_Config_GetGeral(config: Integer; const Buffer: PChar; const BufferLen: Integer): Integer;
  {$IfDef STDCALL}stdcall;{$Else}cdecl;{$EndIf} export;

function NFE_Config_SetGeral(config: Integer; const Value: PChar): Integer;
  {$IfDef STDCALL}stdcall;{$Else}cdecl;{$EndIf} export;

function NFE_Config_GetCertificados(config: Integer; const Buffer: PChar; const BufferLen: Integer): Integer;
  {$IfDef STDCALL}stdcall;{$Else}cdecl;{$EndIf} export;

function NFE_Config_SetCertificados(config: Integer; const Value: PChar): Integer;
  {$IfDef STDCALL}stdcall;{$Else}cdecl;{$EndIf} export;

function NFE_Config_GetArquivos(config: Integer; const Buffer: PChar; const BufferLen: Integer): Integer;
  {$IfDef STDCALL}stdcall;{$Else}cdecl;{$EndIf} export;

function NFE_Config_SetArquivos(config: Integer; const Value: PChar): Integer;
  {$IfDef STDCALL}stdcall;{$Else}cdecl;{$EndIf} export;



NFE_Config_GetEmail,
NFE_Config_SetEmail,
NFE_Config_GetWebService,
NFE_Config_SetWebService,
NFE_Config_GetGeral,
NFE_Config_SetGeral,
NFE_Config_GetCertificados,
NFE_Config_SetCertificados,
NFE_Config_GetArquivos,
NFE_Config_SetArquivos,




function NFE_Config_GetEmail(config: Integer; const Buffer: PChar;
  const BufferLen: Integer): Integer;
  {$IfDef STDCALL}stdcall;{$Else}cdecl;{$EndIf} export;
begin
  if (pNFeDataModule = nil) then
  begin
    Result := -2;
    Exit;
  end;

  try
    pNFeDataModule.Lock.Acquire;

    try

      with pNFeDataModule do
      begin

        {Parametro da Conta de Email}
        with ACBrMail do
        begin
          case config of
            0: StrPLCopy(Buffer, FromName, BufferLen);
            1: StrPLCopy(Buffer, From, BufferLen);
            2: StrPLCopy(Buffer, Host, BufferLen);
            3: StrPLCopy(Buffer, Port, BufferLen);
            4: StrPLCopy(Buffer, Username, BufferLen);
            5: StrPLCopy(Buffer, Password, BufferLen);
            6: StrPLCopy(Buffer, BoolToStr(SetSSL, True), BufferLen);
            7: StrPLCopy(Buffer, BoolToStr(SetTLS, True), BufferLen);
            8: StrPLCopy(Buffer, BoolToStr(ReadingConfirmation, True), BufferLen);
            9: StrPLCopy(Buffer, BoolToStr(UseThread, True), BufferLen);
            10: StrPLCopy(Buffer, IntToStr(Integer(DefaultCharset)), BufferLen);
            else
              raise Exception.Create('Opção ' + IntToStr(config) + ' invalida');
          end;
        end;

      end;

      Result := 0;
    except
      on E: Exception do
      begin
        pNFeDataModule.UltimoErro := E.Message;
        Result := -1;
      end
    end;
  finally
    pNFeDataModule.Lock.Release;
  end;
end;

function NFE_Config_SetEmail(config: Integer; const Value: PChar): Integer;
  cdecl;
 {$IFDEF STDCALL} stdcall; {$ENDIF} {$IFDEF CDECL} cdecl;
{$ENDIF}
var
  StrValue: String;
begin
  if (pNFeDataModule = nil) then
  begin
    Result := -2;
    Exit;
  end;

  try
    pNFeDataModule.Lock.Acquire;
    StrValue := string(Value);

    try

      with pNFeDataModule do
      begin

        {Parametro da Conta de Email}
        with ACBrMail do
        begin
          case config of
            0: FromName := StrValue;
            1: From := StrValue;
            2: Host := StrValue;
            3: Port := StrValue;
            4: Username := StrValue;
            5: Password := StrValue;
            6: SetSSL := StrToBool(StrValue);
            7: SetTLS := StrToBool(StrValue);
            8: ReadingConfirmation := StrToBool(StrValue);
            9: UseThread := StrToBool(StrValue);
            10: DefaultCharset := TMailCharset(StrToInt(StrValue));
            else
              raise Exception.Create('Opção ' + IntToStr(config) + ' invalida');
          end;
        end;

      end;

      Result := 0;
    except
      on E: Exception do
      begin
        pNFeDataModule.UltimoErro := E.Message;
        Result := -1;
      end
    end;
  finally
    pNFeDataModule.Lock.Release;
  end;
end;

function NFE_Config_GetWebService(config: Integer; const Buffer: PChar;
  const BufferLen: Integer): Integer; cdecl;
 {$IFDEF STDCALL} stdcall; {$ENDIF} {$IFDEF CDECL} cdecl;
{$ENDIF}
begin

  if (pNFeDataModule = nil) then
  begin
    Result := -2;
    Exit;
  end;

  try
    pNFeDataModule.Lock.Acquire;
    try
      with pNFeDataModule.ACBrNFe do
      begin
        with Configuracoes.WebServices do
        begin
          case config of
            0: StrPLCopy(Buffer, UF, BufferLen);
            1: StrPLCopy(Buffer, TpAmbToStr(Ambiente), BufferLen);
            2: StrPLCopy(Buffer, BoolToStr(Salvar, True), BufferLen);
            3: StrPLCopy(Buffer, IntToStr(TimeOut), BufferLen);
            4: StrPLCopy(Buffer, BoolToStr(AjustaAguardaConsultaRet, True), BufferLen);
            5: StrPLCopy(Buffer, IntToStr(Integer(TimeZoneConf.ModoDeteccao)), BufferLen);
            6: StrPLCopy(Buffer, TimeZoneConf.TimeZoneStr, BufferLen);
            7: StrPLCopy(Buffer, IntToStr(AguardarConsultaRet), BufferLen);
            8: StrPLCopy(Buffer, IntToStr(Tentativas), BufferLen);
            9: StrPLCopy(Buffer, IntToStr(IntervaloTentativas), BufferLen);
            10: StrPLCopy(Buffer, ProxyHost, BufferLen);
            11: StrPLCopy(Buffer, ProxyPort, BufferLen);
            12: StrPLCopy(Buffer, ProxyUser, BufferLen);
            13: StrPLCopy(Buffer, ProxyPass, BufferLen);
            14: StrPLCopy(Buffer, IntToStr(Integer(SSL.SSLType)), BufferLen);
            else
              raise Exception.Create('Opção ' + IntToStr(config) + ' invalida');
          end;
        end;
      end;
      Result := 0;
    except
      on E: Exception do
      begin
        pNFeDataModule.UltimoErro := E.Message;
        Result := -1;
      end
    end;
  finally
    pNFeDataModule.Lock.Release;
  end;
end;

function NFE_Config_SetWebService(config: Integer; const Value: PChar
  ): Integer;
 {$IfDef STDCALL}stdcall;{$Else}cdecl;{$EndIf} export;
var
  Ok: Boolean;
  StrValue: String;
  IntHelper: Integer;
begin

  if (pNFeDataModule = nil) then
  begin
    Result := -2;
    Exit;
  end;

  try
    pNFeDataModule.Lock.Acquire;
    StrValue := string(Value);
    try
      with pNFeDataModule.ACBrNFe do
      begin
        with Configuracoes.WebServices do
        begin
          case config of
            0: UF := StrValue;
            1: Ambiente := StrToTpAmb(Ok, StrValue);
            2: Salvar := StrToBool(StrValue);
            3:
            begin
              IntHelper := StrToInt(StrValue);
              TimeOut := IfThen(IntHelper < 1000, IntHelper * 1000, IntHelper);
            end;
            4: AjustaAguardaConsultaRet := StrToBool(StrValue);
            5: TimeZoneConf.ModoDeteccao := TTimeZoneModoDeteccao(StrToInt(StrValue));
            6:
            begin
              try
                TimeZoneConf.TimeZoneStr := StrValue;
              except
                TimeZoneConf.TimeZoneStr := GetUTCSistema;
              end;
            end;
            7: AguardarConsultaRet := StrToInt(StrValue);
            8: Tentativas := StrToInt(StrValue);
            9:
            begin
              IntHelper := StrToInt(StrValue);
              IntervaloTentativas :=
                IfThen(IntHelper < 1000, IntHelper * 1000, IntHelper);
            end;
            10: ProxyHost := StrValue;
            11: ProxyPort := StrValue;
            12: ProxyUser := StrValue;
            13: ProxyPass := StrValue;
            14: SSL.SSLType := TSSLType(StrToInt(StrValue));
            else
              raise Exception.Create('Opção ' + IntToStr(config) + ' invalida');
          end;
        end;
      end;
      Result := 0;
    except
      on E: Exception do
      begin
        pNFeDataModule.UltimoErro := E.Message;
        Result := -1;
      end
    end;
  finally
    pNFeDataModule.Lock.Release;
  end;
end;

function NFE_Config_GetGeral(config: Integer; const Buffer: PChar;
  const BufferLen: Integer): Integer; cdecl;
 {$IFDEF STDCALL} stdcall; {$ENDIF} {$IFDEF CDECL} cdecl;
{$ENDIF}
begin

  if (pNFeDataModule = nil) then
  begin
    Result := -2;
    Exit;
  end;

  try
    pNFeDataModule.Lock.Acquire;
    try
      with pNFeDataModule.ACBrNFe.Configuracoes.Geral do
      begin
        case config of
          0: StrPLCopy(Buffer, IntToStr(Integer(ModeloDF)), BufferLen);
          1: StrPLCopy(Buffer, IntToStr(Integer(VersaoDF)), BufferLen);
          2: StrPLCopy(Buffer, BoolToStr(AtualizarXMLCancelado, True), BufferLen);
          3: StrPLCopy(Buffer, IdCSC, BufferLen);
          4: StrPLCopy(Buffer, CSC, BufferLen);
          5: StrPLCopy(Buffer, BoolToStr(IncluirQRCodeXMLNFCe, True), BufferLen);
          6: StrPLCopy(Buffer, IntToStr(Integer(SSLLib)), BufferLen);
          7: StrPLCopy(Buffer, IntToStr(Integer(SSLCryptLib)), BufferLen);
          8: StrPLCopy(Buffer, IntToStr(Integer(SSLHttpLib)), BufferLen);
          9: StrPLCopy(Buffer, IntToStr(Integer(SSLXmlSignLib)), BufferLen);
          10: StrPLCopy(Buffer, IntToStr(Integer(FormaEmissao)), BufferLen);
          11: StrPLCopy(Buffer, BoolToStr(Salvar, True), BufferLen);
          12: StrPLCopy(Buffer, BoolToStr(ExibirErroSchema, True), BufferLen);
          13: StrPLCopy(Buffer, FormatoAlerta, BufferLen);
          14: StrPLCopy(Buffer, BoolToStr(RetirarAcentos, True), BufferLen);
          15: StrPLCopy(Buffer, BoolToStr(IdentarXML, True), BufferLen);
          16: StrPLCopy(Buffer, BoolToStr(ValidarDigest, True), BufferLen);
          else
            raise Exception.Create('Opção ' + IntToStr(config) + ' invalida');
        end;
      end;
      Result := 0;
    except
      on E: Exception do
      begin
        pNFeDataModule.UltimoErro := E.Message;
        Result := -1;
      end
    end;
  finally
    pNFeDataModule.Lock.Release;
  end;
end;

function NFE_Config_SetGeral(config: Integer; const Value: PChar): Integer;
  cdecl;
 {$IFDEF STDCALL} stdcall; {$ENDIF} {$IFDEF CDECL} cdecl;
{$ENDIF}
var
  StrValue: String;
begin

  if (pNFeDataModule = nil) then
  begin
    Result := -2;
    Exit;
  end;

  try
    pNFeDataModule.Lock.Acquire;
    StrValue := string(Value);
    try
      with pNFeDataModule.ACBrNFe.Configuracoes.Geral do
      begin
        case config of
          0: ModeloDF := TpcnModeloDF(StrToInt(StrValue));
          1: VersaoDF := TpcnVersaoDF(StrToInt(StrValue));
          2: AtualizarXMLCancelado := StrToBool(StrValue);
          3: IdCSC := StrValue;
          4: CSC := StrValue;
          5: IncluirQRCodeXMLNFCe := StrToBool(StrValue);
          6: SSLLib := TSSLLib(StrToInt(StrValue));
          7: SSLCryptLib := TSSLCryptLib(StrToInt(StrValue));
          8: SSLHttpLib := TSSLHttpLib(StrToInt(StrValue));
          9: SSLXmlSignLib := TSSLXmlSignLib(StrToInt(StrValue));
          10: FormaEmissao := TpcnTipoEmissao(StrToInt(StrValue));
          11: Salvar := StrToBool(StrValue);
          12: ExibirErroSchema := StrToBool(StrValue);
          13: FormatoAlerta := StrValue;
          14: RetirarAcentos := StrToBool(StrValue);
          15: IdentarXML := StrToBool(StrValue);
          16: ValidarDigest := StrToBool(StrValue);
          else
            raise Exception.Create('Opção ' + IntToStr(config) + ' invalida');
        end;
      end;
      Result := 0;
    except
      on E: Exception do
      begin
        pNFeDataModule.UltimoErro := E.Message;
        Result := -1;
      end
    end;
  finally
    pNFeDataModule.Lock.Release;
  end;
end;

function NFE_Config_GetCertificados(config: Integer; const Buffer: PChar;
  const BufferLen: Integer): Integer;
  {$IfDef STDCALL}stdcall;{$Else}cdecl;{$EndIf} export;
begin

  if (pNFeDataModule = nil) then
  begin
    Result := -2;
    Exit;
  end;

  try
    pNFeDataModule.Lock.Acquire;
    try
      with pNFeDataModule.ACBrNFe.Configuracoes.Certificados do
      begin
        case config of
          0: StrPLCopy(Buffer, ArquivoPFX, BufferLen);
          1: StrPLCopy(Buffer, DadosPFX, BufferLen);
          2: StrPLCopy(Buffer, NumeroSerie, BufferLen);
          3: StrPLCopy(Buffer, Senha, BufferLen);
          4: StrPLCopy(Buffer, BoolToStr(VerificarValidade, True), BufferLen);
          else
            raise Exception.Create('Opção ' + IntToStr(config) + ' invalida');
        end;
      end;
      Result := 0;
    except
      on E: Exception do
      begin
        pNFeDataModule.UltimoErro := E.Message;
        Result := -1;
      end
    end;
  finally
    pNFeDataModule.Lock.Release;
  end;
end;

function NFE_Config_SetCertificados(config: Integer; const Value: PChar
  ): Integer;
  {$IfDef STDCALL}stdcall;{$Else}cdecl;{$EndIf} export;
var
  StrValue: String;
begin

  if (pNFeDataModule = nil) then
  begin
    Result := -2;
    Exit;
  end;

  try
    pNFeDataModule.Lock.Acquire;
    StrValue := string(Value);
    try
      with pNFeDataModule.ACBrNFe.Configuracoes.Certificados do
      begin
        case config of
          0: ArquivoPFX := StrValue;
          1: DadosPFX := StrValue;
          2: NumeroSerie := StrValue;
          3: Senha := StrValue;
          4: VerificarValidade := StrToBool(StrValue);
          else
            raise Exception.Create('Opção ' + IntToStr(config) + ' invalida');
        end;
      end;
      Result := 0;
    except
      on E: Exception do
      begin
        pNFeDataModule.UltimoErro := E.Message;
        Result := -1;
      end
    end;
  finally
    pNFeDataModule.Lock.Release;
  end;
end;

function NFE_Config_GetArquivos(config: Integer; const Buffer: PChar;
  const BufferLen: Integer): Integer;
  {$IfDef STDCALL}stdcall;{$Else}cdecl;{$EndIf} export;
begin

  if (pNFeDataModule = nil) then
  begin
    Result := -2;
    Exit;
  end;

  try
    pNFeDataModule.Lock.Acquire;
    try
      with pNFeDataModule.ACBrNFe.Configuracoes.Arquivos do
      begin
        case config of
          0: StrPLCopy(Buffer, PathSalvar, BufferLen);
          1: StrPLCopy(Buffer, PathSchemas, BufferLen);
          2: StrPLCopy(Buffer, IniServicos, BufferLen);
          3: StrPLCopy(Buffer, BoolToStr(Salvar, True), BufferLen);
          4: StrPLCopy(Buffer, BoolToStr(AdicionarLiteral, True), BufferLen);
          5: StrPLCopy(Buffer, BoolToStr(SepararPorCNPJ, True), BufferLen);
          6: StrPLCopy(Buffer, BoolToStr(SepararPorModelo, True), BufferLen);
          7: StrPLCopy(Buffer, BoolToStr(SepararPorAno, True), BufferLen);
          8: StrPLCopy(Buffer, BoolToStr(SepararPorMes, True), BufferLen);
          9: StrPLCopy(Buffer, BoolToStr(SepararPorDia, True), BufferLen);
          10: StrPLCopy(Buffer, BoolToStr(EmissaoPathNFe, True), BufferLen);
          11: StrPLCopy(Buffer, BoolToStr(SalvarEvento, True), BufferLen);
          12: StrPLCopy(Buffer, BoolToStr(SalvarApenasNFeProcessadas, True), BufferLen);
          13: StrPLCopy(Buffer, PathNFe, BufferLen);
          14: StrPLCopy(Buffer, PathInu, BufferLen);
          15: StrPLCopy(Buffer, PathEvento, BufferLen);
          16: StrPLCopy(Buffer, DownloadNFe.PathDownload, BufferLen);
          17: StrPLCopy(Buffer, BoolToStr(DownloadNFe.SepararPorNome, True), BufferLen);
          else
            raise Exception.Create('Opção ' + IntToStr(config) + ' invalida');
        end;
      end;
      Result := 0;
    except
      on E: Exception do
      begin
        pNFeDataModule.UltimoErro := E.Message;
        Result := -1;
      end
    end;
  finally
    pNFeDataModule.Lock.Release;
  end;
end;

function NFE_Config_SetArquivos(config: Integer; const Value: PChar): Integer;
  {$IfDef STDCALL}stdcall;{$Else}cdecl;{$EndIf} export;
var
  StrValue: String;
begin

  if (pNFeDataModule = nil) then
  begin
    Result := -2;
    Exit;
  end;

  try
    pNFeDataModule.Lock.Acquire;
    StrValue := string(Value);
    try
      with pNFeDataModule.ACBrNFe.Configuracoes.Arquivos do
      begin
        case config of
          0: PathSalvar := StrValue;
          1: PathSchemas := StrValue;
          2: IniServicos := StrValue;
          3: Salvar := StrToBool(StrValue);
          4: AdicionarLiteral := StrToBool(StrValue);
          5: SepararPorCNPJ := StrToBool(StrValue);
          6: SepararPorModelo := StrToBool(StrValue);
          7: SepararPorAno := StrToBool(StrValue);
          8: SepararPorMes := StrToBool(StrValue);
          9: SepararPorDia := StrToBool(StrValue);
          10: EmissaoPathNFe := StrToBool(StrValue);
          11: SalvarEvento := StrToBool(StrValue);
          12: SalvarApenasNFeProcessadas := StrToBool(StrValue);
          13: PathNFe := StrValue;
          14: PathInu := StrValue;
          15: PathEvento := StrValue;
          16: DownloadNFe.PathDownload := StrValue;
          17: DownloadNFe.SepararPorNome := StrToBool(StrValue);
          else
            raise Exception.Create('Opção ' + IntToStr(config) + ' invalida');
        end;
      end;
      Result := 0;
    except
      on E: Exception do
      begin
        pNFeDataModule.UltimoErro := E.Message;
        Result := -1;
      end
    end;
  finally
    pNFeDataModule.Lock.Release;
  end;
end;


