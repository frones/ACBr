{******************************************************************************}
{ Projeto: ACBr Monitor                                                        }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para }
{ criar uma interface de comunicação com equipamentos de automacao comercial.  }
{                                                                              }
{ Direitos Autorais Reservados (c) 2010 Daniel SimÃµes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na página do Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Este programa é software livre; você pode redistribuí-lo e/ou modificá-lo   }
{ sob os termos da Licença Pública Geral GNU, conforme publicada pela Free     }
{ Software Foundation; tanto a versão 2 da Licença como (a seu critério)       }
{ qualquer versão mais nova.                                                   }
{                                                                              }
{  Este programa é distribuído na expectativa de ser útil, mas SEM NENHUMA     }
{ GARANTIA; nem mesmo a garantia implícita de COMERCIALIZAÇÃO OU DE ADEQUAÇÃO A}
{ QUALQUER PROPÓSITO EM PARTICULAR. Consulte a Licença Pública Geral GNU para  }
{ obter mais detalhes. (Arquivo LICENCA.TXT ou LICENSE.TXT)                    }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral GNU junto com este}
{ programa; se não, escreva para a Free Software Foundation, Inc., 59 Temple   }
{ Place, Suite 330, Boston, MA 02111-1307, USA. Você também pode obter uma     }
{ copia da licença em:  http://www.opensource.org/licenses/gpl-license.php     }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}
unit ACBrMonitorConsoleDM;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  CmdUnit , blcksock,
  ACBrUtil, ACBrLCB, ACBrDIS, ACBrGAV, ACBrDevice, ACBrCHQ,
  ACBrECF, ACBrRFD, ACBrBAL, ACBrETQ, ACBrSocket, ACBrCEP, 
  ACBrIBGE, ACBrMail, ACBrSedex,ACBrNCMs ;

const
   {$I versao.txt}
   _C = 'tYk*5W@' ;

type

  { Tdm }

  Tdm = class(TDataModule)
    ACBrCEP1 : TACBrCEP ;
    ACBrIBGE1 : TACBrIBGE ;
    ACBrMail1: TACBrMail;
    TcpServer: TACBrTCPServer;
    ACBrCHQ1: TACBrCHQ;
    ACBrGAV1: TACBrGAV;
    ACBrDIS1: TACBrDIS;
    ACBrLCB1: TACBrLCB;
    ACBrRFD1: TACBrRFD;
    ACBrBAL1: TACBrBAL;
    ACBrETQ1: TACBrETQ;
    ACBrSedex1:TACBrSedex;
    ACBrNCMs1: TACBrNCMs;
    procedure ACBrCEP1AntesAbrirHTTP(var AURL : String) ;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure ACBrLCB1LeCodigo(Sender: TObject);
    procedure ACBrRFD1GetKeyRSA(var PrivateKey_RSA: String);
    procedure TcpServerConecta(const TCPBlockSocket : TTCPBlockSocket ;
      var {%H-}Enviar : AnsiString) ;
    procedure TcpServerDesConecta(const TCPBlockSocket : TTCPBlockSocket ;
      {%H-}Erro : Integer ; {%H-}ErroDesc : String) ;
    procedure TcpServerRecebeDados(const TCPBlockSocket : TTCPBlockSocket ;
      const Recebido : AnsiString ; var {%H-}Enviar : AnsiString) ;
  private
    fChaveBuscarCEP : String ;
    fsDisWorking: Boolean;
    fsOldIntervaloLCB:Integer ;
    fsSWPwd : String ;

    procedure SetDisWorking(const Value: Boolean);
    procedure LerSW;
    function LerChaveSWH: String;
    { Private declarations }
  public
    { Public declarations }
    Conexao  : TTCPBlockSocket ;
    ACBrECF1 : TACBrECF ;
    Cmd : TACBrCmd ;
    ComandosAProcessar : TStringList ;
    ArqSaiTXT, ArqSaiTMP, ArqEntTXT, ArqLogTXT : String ;
    Intervalo, LinhasLog : Integer ;
    PermiteComandos, VerificaCheque, GravarLog, IsTCP, IsTXT, ConvENT, ConvSAI : Boolean ;
    LCBIntervalo : Integer ;
    LCBTeclado : Boolean ;
    LCBSufixoIncluir : String ;
    LCBDispositivo : String ;
    TipoCMD : String ;

    property DISWorking : Boolean read fsDisWorking write SetDisWorking ;
    property ChaveBuscarCEP : String read fChaveBuscarCEP ;

    procedure LerIni;
    procedure CriarIniDefault;
    procedure AjustaLinhasLog;
    procedure Processar;
    procedure Resposta(Comando, Resposta: string);

    procedure ACBrECF1MsgPoucoPapel(Sender: TObject);
    procedure ACBrECF1AguardandoRespostaChange(Sender: TObject);
  end;

var
  dm: Tdm;

implementation
Uses IniFiles, UtilUnit,
     {$IFDEF MSWINDOWS} sndkey32, {$ENDIF}
     {$IFDEF LINUX} unix, baseunix, termio, {$ENDIF}
     DoACBrUnit, DoECFUnit, DoGAVUnit, DoCHQUnit, DoDISUnit, DoLCBUnit,
     DoBALUnit , DoETQUnit, DoCEPUnit, DoIBGEUnit, DoEmailUnit, DoSedexUnit,DoNcmUnit,
     typinfo;

{$R *.lfm}

procedure Tdm.DataModuleCreate(Sender: TObject);
begin
  { Cria o ECF manualmente para não causar erros nas propriedades CONSOLE }
  ACBrECF1  := TACBrECF.Create(Self);
  Cmd       := TACBrCmd.Create ;
  ComandosAProcessar := TStringList.Create ;

  ArqSaiTXT := '' ;
  ArqSaiTMP := '' ;
  ArqEntTXT := '' ;
  ArqLogTXT := '' ;
  Conexao   := nil ;
  fsOldIntervaloLCB:=0 ;
  DISWorking:= false ;
  fsSWPwd   := '' ;

  TipoCMD   := 'A' ; {Tipo de Comando A - ACBr, B - Bematech, D - Daruma}
end;

procedure Tdm.ACBrCEP1AntesAbrirHTTP(var AURL : String) ;
begin
  if (ACBrCEP1.WebService = wsBuscarCep) and (fChaveBuscarCEP <> '') then
  begin
    AURL := AURL + '&chave='+fChaveBuscarCEP;
  end ;
end;

{------------------------------------------------------------------------------}
procedure Tdm.DataModuleDestroy(Sender: TObject);
begin
  ACBrECF1.Free ;
  Cmd.Free ;
  ComandosAProcessar.Free ;
end;


{------------------------------------------------------------------------------}
procedure Tdm.ACBrECF1MsgPoucoPapel(Sender: TObject);
begin
  Writeln( 'ATENÇAO. Pouco papel' ) ;
end;

{------------------------------------------------------------------------------}
procedure Tdm.ACBrECF1AguardandoRespostaChange( Sender: TObject);
begin
  { ECF sendo usado junto LCB, deve desabilitar o LCB enquando o ECF estiver
    ocupado imprimindo, para evitar de enviar códigos na hora indevida, como
    por exemplo, quando o EDIT / GET do Campos código não está com o FOCO }
  if ACBrLCB1.Ativo then
     if ACBrECF1.AguardandoResposta then
        ACBrLCB1.Intervalo := 0
     else
        ACBrLCB1.Intervalo := LCBIntervalo ;
end;

{------------------------------------------------------------------------------}
procedure Tdm.AjustaLinhasLog ;
Var LogNew, LogOld : TStringList ;
    I : Integer ;
begin
  if (LinhasLog <= 0) or (not GravarLog) or
     (not FileExists( ArqLogTXT )) then
     exit ;

  LogOld := TStringList.Create ;
  try
     LogOld.LoadFromFile( ArqLogTXT );
     if LogOld.Count > LinhasLog then
     begin
        WriteLn('Ajustando o tamanho do arquivo: '+ArqLogTXT) ;
        WriteLn('Numero de Linhas atual: '+IntToStr(LogOld.Count) ) ;
        WriteLn('Reduzindo para: '+IntToStr(LinhasLog)+ ' linhas' );

        { Se for muito grande é mais rápido copiar para outra lista do que Deletar }
        if (LogOld.Count - LinhasLog) > LinhasLog then
         begin
           LogNew := TStringList.Create ;
           try
              LogNew.Clear ;

              For I := LinhasLog downto 1 do
                 LogNew.Add(LogOld[ LogOld.Count - I ] ) ;

              LogNew.SaveToFile( ArqLogTXT );
           finally
              LogNew.Free ;
           end ;
         end
        else
         begin
           { Existe alguma maneira mais rápida de fazer isso ??? }
           LogOld.BeginUpdate ;
           while LogOld.Count > LinhasLog do
              LogOld.Delete(0) ;
           LogOld.EndUpdate ;
           LogOld.SaveToFile( ArqLogTXT );
         end ;
     end ;
  finally
     LogOld.Free ;
  end ;
end ;

{------------------------------------------------------------------------------}
procedure Tdm.LerIni ;
Var ACBrMonitorINI, ECFDeviceParams : string;
    Ini : TIniFile ;
begin
  { ---------------- Inicializando o ACBrMonitorConsole ---------------- }
  ACBrMonitorINI := ExtractFilePath( ParamStr(0) )+ 'ACBrMonitor.ini';

  if not FileExists( ACBrMonitorINI ) then
     CriarIniDefault ;

  { ---------------- Lendo o arquivo INI ---------------- }
  Ini := TIniFile.Create( ACBrMonitorINI ) ;
  try
     IsTCP := Ini.ReadBool('ACBrMonitor','Modo_TCP',false);
     IsTXT := not IsTCP ;

    dm.TcpServer.Port    := Ini.ReadString('ACBrMonitor','TCP_Porta','3434');
    dm.TcpServer.TimeOut := Ini.ReadInteger('ACBrMonitor','TCP_TimeOut',10000);

    ArqEntTXT := AcertaPath( Ini.ReadString('ACBrMonitor','TXT_Entrada','ENT.TXT') ) ;
    ArqSaiTXT := AcertaPath( Ini.ReadString('ACBrMonitor','TXT_Saida','SAI.TXT') ) ;
    ConvENT   := Ini.ReadBool('ACBrMonitor', 'Converte_TXT_Entrada_Ansi', False);
    ConvSAI   := Ini.ReadBool('ACBrMonitor', 'Converte_TXT_Saida_Ansi', False);
    ArqSaiTMP := ChangeFileExt( ArqSaiTXT, '.tmp' ) ;
    ArqLogTXT := AcertaPath( Ini.ReadString('ACBrMonitor','Arquivo_Log','LOG.TXT') ) ;

    Intervalo := Ini.ReadInteger('ACBrMonitor','Intervalo',50);

    GravarLog := Ini.ReadBool('ACBrMonitor','Gravar_Log',false) and ( ArqLogTXT <> '' ) ;
    LinhasLog := Ini.ReadInteger('ACBrMonitor','Linhas_Log',0);

    PermiteComandos := Ini.ReadBool('ACBrMonitor','Comandos_Remotos',false);

    with ACBrECF1 do
    begin
       Desativar ;
       Modelo               := TACBrECFModelo( Ini.ReadInteger('ECF','Modelo',0) ) ;
       Porta                := Ini.ReadString('ECF','Porta','Procurar');
       TimeOut              := Ini.ReadInteger('ECF','Timeout',3);
       ArredondaPorQtd      := Ini.ReadBool('ECF','ArredondamentoPorQtd',false);
       DescricaoGrande      := Ini.ReadBool('ECF','DescricaoGrande',True);
       GavetaSinalInvertido := Ini.ReadBool('ECF','GavetaSinalInvertido',false);
       BloqueiaMouseTeclado := False ;
       ExibeMensagem        := False ;
       ReTentar             := False ;
       IntervaloAposComando := Ini.ReadInteger('ECF','IntervaloAposComando',100);
       ArqLOG               := Ini.ReadString('ECF','ArqLog','');
       ECFDeviceParams      := Ini.ReadString('ECF','SerialParams','');
       if ECFDeviceParams <> '' then
          Device.ParamsString  := ECFDeviceParams ;

       OnMsgPoucoPapel            := @ACBrECF1MsgPoucoPapel ;
       OnAguardandoRespostaChange := @ACBrECF1AguardandoRespostaChange ;
    end ;

    with ACBrCHQ1 do
    begin
       Desativar ;
       Modelo  := TACBrCHQModelo( Ini.ReadInteger('CHQ','Modelo',0) ) ;
       Porta   := Ini.ReadString('CHQ','Porta','');
       Favorecido := Ini.ReadString('CHQ','Favorecido','');
       Cidade     := Ini.ReadString('CHQ','Cidade','');
       VerificaCheque := Ini.ReadBool('CHQ','VerificaFormulario',false);
       if Ini.ReadString('CHQ','PathBemafiINI','') <> '' then
       begin
          try
             ArquivoBemaFiINI := Ini.ReadString('CHQ','PathBemafiINI','');
             WriteLn( 'Arquivo de Cheques: '+ArquivoBemaFiINI + ' lido com sucesso.') ;
          except
             on E : Exception do
                WriteLn( E.Message );
          end ;
       end ;
    end ;

    with ACBrGAV1 do
    begin
       Desativar ;
       StrComando         := Ini.ReadString('GAV','StringAbertura','');
       AberturaIntervalo  := Ini.ReadInteger('GAV','AberturaIntervalo',
          AberturaIntervalo);
       AberturaAntecipada := TACBrGAVAberturaAntecipada(
          Ini.ReadInteger('GAV','AcaoAberturaAntecipada',1) ) ;
       Modelo     := TACBrGAVModelo( Ini.ReadInteger('GAV','Modelo',0) ) ;
       Porta      := Ini.ReadString('GAV','Porta','');
       Ativo      := (pos('serial',LowerCase(ModeloStr)) > 0) ;
    end ;

    with ACBrDIS1 do
    begin
       Desativar ;
       Intervalo := Ini.ReadInteger('DIS','Intervalo',300) ;
       Passos    := Ini.ReadInteger('DIS','Passos',1) ;
       Modelo    := TACBrDISModelo( Ini.ReadInteger('DIS','Modelo',0) ) ;
       Porta     := Ini.ReadString('DIS','Porta','');
       IntervaloEnvioBytes := Ini.ReadInteger('DIS','IntervaloEnvioBytes',3);
    end ;

    with ACBrLCB1 do
    begin
       Desativar ;
       Porta           := Ini.ReadString('LCB','Porta','Sem Leitor');
       Intervalo       := Ini.ReadInteger('LCB','Intervalo',100);
       fsOldIntervaloLCB:=Intervalo ;
       Sufixo          := Ini.ReadString('LCB','SufixoLeitor','#13');
       ExcluirSufixo   := Ini.ReadBool('LCB','ExcluirSufixo',false) ;
       PrefixoAExcluir := Ini.ReadString('LCB','PrefixoAExcluir','');
       UsarFila        := not Ini.ReadBool('LCB','Teclado',True) ;
       Device.ParamsString:= Ini.ReadString('LCB','Device','') ;

       LCBIntervalo     := Intervalo ;
       LCBTeclado       := Ini.ReadBool('LCB','Teclado',True) ;
       LCBSufixoIncluir := Trim(Ini.ReadString('LCB','SufixoIncluir','')) ;
       LCBDispositivo   := Trim(Ini.ReadString('LCB','Dispositivo','')) ;

       if (Porta <> 'Sem Leitor') and (Porta <> '') then
          Ativar ;
    end ;

    with ACBrRFD1 do
    begin
       DirRFD := INI.ReadString('RFD','DirRFD','') ;

       if INI.ReadBool('RFD','GerarRFD',False) then
        begin
          ACBrECF1.RFD := ACBrRFD1 ;
          LerSW ;
        end
       else
          ACBrECF1.RFD := nil ;
    end ;

    with ACBrBAL1 do
    begin
       Desativar ;
       Intervalo := Ini.ReadInteger('BAL','Intervalo',200) ;
       Modelo    := TACBrBALModelo( Ini.ReadInteger('BAL','Modelo',0) ) ;
       Porta     := Ini.ReadString('BAL','Porta','');
    end ;

    with ACBrCEP1 do
    begin
      WebService := TACBrCEPWebService( Ini.ReadInteger('CEP', 'WebService', 0) ) ;
      fChaveBuscarCEP := Ini.ReadString('CEP', 'Chave_BuscarCEP', '');
      ProxyHost  := Ini.ReadString('CEP', 'Proxy_Host', '');
      ProxyPort  := Ini.ReadString('CEP', 'Proxy_Port', '');
      ProxyUser  := Ini.ReadString('CEP', 'Proxy_User', '');
      ProxyPass  := LeINICrypt(INI, 'CEP', 'Proxy_Pass', _C) ;
    end ;

    with ACBrMail1 do
    begin
      FromName       := Ini.ReadString('EMAIL', 'NomeExibicao', '');
      From           := Ini.ReadString('EMAIL', 'Endereco', '');
      Host           := Ini.ReadString('EMAIL', 'Email', '');
      Port           := Ini.ReadString('EMAIL','Porta', '0');
      Username       := LeINICrypt(Ini,'EMAIL', 'Usuario', _C);
      Password       := LeINICrypt(Ini,'EMAIL', 'Senha', _C);
      SetSSL         := Ini.ReadBool('EMAIL', 'ExigeSSL', False);
      SetTLS         := Ini.ReadBool('EMAIL', 'ExigeTLS', False);
      DefaultCharset := TMailCharset(GetEnumValue(TypeInfo(TMailCharset),
                        Ini.ReadString('EMAIL', 'Codificacao', '')));
    end;

    with ACBrIBGE1 do
    begin
      ProxyHost  := Ini.ReadString('CEP', 'Proxy_Host', '');
      ProxyPort  := Ini.ReadString('CEP', 'Proxy_Port', '');
      ProxyUser  := Ini.ReadString('CEP', 'Proxy_User', '');
      ProxyPass  := LeINICrypt(INI, 'CEP', 'Proxy_Pass', _C) ;
    end ;
 finally
    Ini.Free ;
 end ;
end ;

{------------------------------------------------------------------------------}
procedure Tdm.LerSW;
  Var INI : TIniFile ;
      ArqSWH, MonitorINI, Hash : String ;
begin
  ArqSWH     := ExtractFilePath( ParamStr(0) )+'swh.ini' ;
  MonitorINI := ExtractFilePath( ParamStr(0) )+'ACBrMonitor.ini';

  if not (FileExists( ArqSWH ) and FileExists( MonitorINI ) ) then
     exit ;

  Ini := TIniFile.Create( MonitorINI ) ;
  try
     Hash := IntToStrZero( StrToIntDef( OnlyNumber(
                           LeINICrypt(INI,'ACBrMonitor','HashSenha', _C)), -1),8) ;
  finally
     INI.Free ;
  end ;

  Ini := TIniFile.Create( ArqSWH ) ;
  try
     ACBrRFD1.SH_CNPJ := LeINICrypt(INI,'SWH','CNPJ', Hash );
     fsSWPwd := IntToStrZero( StringCrc16(ACBrRFD1.SH_CNPJ + Hash),8) ;

     if LeINICrypt(INI,'SWH','Verifica', fsSWPwd) <> 'ARQUIVO SWH.INI ESTA OK' then
        raise Exception.Create('Arquivo "swh.ini" inválido.') ;

     ACBrRFD1.SH_RazaoSocial      := LeINICrypt(INI,'SWH','RazaoSocial', fsSWPwd);
     ACBrRFD1.SH_COO              := LeINICrypt(INI,'SWH','COO', fsSWPwd);
     ACBrRFD1.SH_IE               := LeINICrypt(INI,'SWH','IE', fsSWPwd);
     ACBrRFD1.SH_IM               := LeINICrypt(INI,'SWH','IM', fsSWPwd);
     ACBrRFD1.SH_NomeAplicativo   := LeINICrypt(INI,'SWH','Aplicativo', fsSWPwd);
     ACBrRFD1.SH_NumeroAplicativo := LeINICrypt(INI,'SWH','NumeroAplicativo', fsSWPwd);
     ACBrRFD1.SH_VersaoAplicativo := LeINICrypt(INI,'SWH','VersaoAplicativo', fsSWPwd);
     ACBrRFD1.SH_Linha1           := LeINICrypt(INI,'SWH','Linha1', fsSWPwd);
     ACBrRFD1.SH_Linha2           := LeINICrypt(INI,'SWH','Linha2', fsSWPwd);
  finally
     Ini.Free ;
  end ;
end;

{------------------------------------------------------------------------------}
Function Tdm.LerChaveSWH : String;
  Var INI : TIniFile ;
begin
  Result := '' ;
  Ini := TIniFile.Create( ExtractFilePath( ParamStr(0) )+'swh.ini' ) ;
  try
     Result := Trim( LeINICrypt(INI,'SWH','RSA', fsSWPwd) );
  finally
     Ini.Free ;
  end ;
end ;

{------------------------------------------------------------------------------}
Procedure Tdm.Processar ;
var
  Linha : string;
begin
  while ComandosAProcessar.Count > 0 do
  begin
     Linha := Trim( ComandosAProcessar[0] );
     ComandosAProcessar.Delete(0);

     if Linha <> '' then
     begin
        WriteLn( 'Processando: '+Linha ) ;

        try
           if pos('.',Linha) = 0 then              { Comandos do ACBrMonitor }
              Linha := 'ACBR.'+Linha ;

           Cmd.Comando := Linha ;

           if Cmd.Objeto = 'ECF' then
              DoECF( Cmd )
           else if Cmd.Objeto = 'ACBR' then
              DoACBr( Cmd )
           else if Cmd.Objeto = 'GAV' then
              DoGAV( Cmd )
           else if Cmd.Objeto = 'CHQ' then
              DoCHQ( Cmd )
           else if Cmd.Objeto = 'DIS' then
              DoDIS( Cmd )
           else if Cmd.Objeto = 'LCB' then
              DoLCB( Cmd )
           else if Cmd.Objeto = 'BAL' then
              DoBAL( Cmd )
           else if Cmd.Objeto = 'ETQ' then
              DoETQ( Cmd )
           else if Cmd.Objeto = 'CEP' then
             DoCEP( Cmd )
           else if Cmd.Objeto = 'IBGE' then
             DoIBGE( Cmd )
           else if Cmd.Objeto = 'EMAIL' then
             DoEmail ( Cmd )
           else if Cmd.Objeto = 'SEDEX' then
             DoSedex(Cmd)
           else if Cmd.Objeto = 'NCM' then
             DoNcm(Cmd);

           Resposta(Linha, 'OK: '+Cmd.Resposta );
        except
           on E : Exception do
              Resposta(Linha, 'ERRO: '+E.Message );
        end ;
     end ;
  end ;
end ;

Procedure Tdm.Resposta(Comando, Resposta : string);
begin
  if IsTCP then
  begin
     if Assigned( dm.Conexao ) then
     begin
        Resposta := StringReplace(Resposta, chr(3), '', [rfReplaceAll]);

        dm.Conexao.SendString(Resposta);
        dm.Conexao.SendByte(3);
     end ;
  end ;

  if IsTXT then
  begin
     { Primeiro salva em Temporário para que a gravação de todos os Bytes ocorra
       antes que a aplicação controladora do ACBrMonitor tente ler o arquivo de
       Resposta incompleto }
     DeleteFile( ArqSaiTMP ) ;

     if FileExists(ArqSaiTXT) then
        RenameFile(ArqSaiTXT, ArqSaiTMP) ; { GravaArqResp faz append se arq. existir }

     if TipoCMD = 'A' then
     begin
        if ConvSAI then
           Resposta := Utf8ToAnsi(Resposta);
        WriteToTXT(ArqSaiTMP, Resposta);
        RenameFile(ArqSaiTMP, ArqSaiTXT) ;
     end
     else if TipoCMD = 'B' then
     begin
        if copy(Resposta,1,3) <> 'OK:' then
        begin
           WriteToTXT(ExtractFilePath(ArqSaiTMP)+'STATUS.TXT','0,0,0') ;
        end
        else
        begin
           WriteToTXT(ExtractFilePath(ArqSaiTMP)+'STATUS.TXT','6,0,0') ;
           Resposta := StringReplace( Resposta, 'OK: ', '',[rfReplaceAll]) ;
           Resposta := StringReplace( Resposta, '/', '',[rfReplaceAll]) ;
           Resposta := StringReplace( Resposta, ':', '',[rfReplaceAll]) ;
           WriteToTXT(ArqSaiTMP,Resposta) ;
           RenameFile(ArqSaiTMP, ArqSaiTXT) ;
        end
     end
     else if TipoCMD = 'D' then
     begin
        if copy(Resposta,1,3) <> 'OK:' then
        begin
           WriteToTXT(ExtractFilePath(ArqSaiTMP)+'DARUMA.RET','-27;006;000;000') ;
        end
        else
        begin
           Resposta := StringReplace( Resposta, 'OK: ', '',[rfReplaceAll]) ;
           Resposta := StringReplace( Resposta, '/', '',[rfReplaceAll]) ;
           Resposta := StringReplace( Resposta, ':', '',[rfReplaceAll]) ;
           Resposta := '001;006;000;000;'+Resposta ;
           WriteToTXT(ArqSaiTMP,Resposta) ;
           RenameFile(ArqSaiTMP, ExtractFilePath(ArqSaiTMP)+'DARUMA.RET') ;
        end
     end
     
  end ;


  WriteLn( {Comando + sLineBreak + }Resposta ) ;

  if GravarLog then
     WriteToTXT(ArqLogTXT, Comando + sLineBreak + Resposta);
end ;


procedure Tdm.CriarIniDefault;
Var Memo : TStringList ;
begin

  Memo := TStringList.Create ;
  try
     Memo.Clear ;

     Memo.Add('******************************************************************************'+sLineBreak+
              '* Projeto: ACBr Monitor Console                                              *'+sLineBreak+
              '*                                                                            *'+sLineBreak+
              '* Direitos Autorais Reservados (c) 2010 Daniel SimÃµes de Almeida             *'+sLineBreak+
              '*                                                                            *'+sLineBreak+
              '*  Você pode obter a última versão desse arquivo na página do Projeto ACBr   *'+sLineBreak+
              '* Componentes localizado em      http://www.sourceforge.net/projects/acbr    *'+sLineBreak+
              '*                                                                            *'+sLineBreak+
              '* Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br*'+sLineBreak+
              '*              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                *'+sLineBreak+
              '******************************************************************************'+sLineBreak+sLineBreak );

     Memo.Add('******************************************************************************'+sLineBreak+
              '*   Para maiores expliçoes sobre esse arquivo execute o programa ACBrMonitor *'+sLineBreak+
              '* e clique em CONFIGURAR                                                     *'+sLineBreak+
              '*                                                                            *'+sLineBreak+
              '* Valores Boleanos use: 0 = False, 1 = True                                  *'+sLineBreak+
              '*                                                                            *'+sLineBreak+
              '* Modelos de ECF:                                                            *'+sLineBreak+
              '*   0 = ecfNenhum,    1 = ecfNaoFiscal, 2 = ecfBematech, 3 = ecfSweda,       *'+sLineBreak+
              '*   4 = ecfDaruma,    5 = ecfSchalter,  6 = ecfMecaf,    7 = ecfYanco,       *'+sLineBreak+
              '*   8 = ecfDataRegis, 9 = ecfUrano,     10=ecfICash,     11=ecfQuattro,      *'+sLineBreak+
              '*   12= ecfFiscNET                                                           *'+sLineBreak+
              '*                                                                            *'+sLineBreak+
              '* Modelos de CHQ                                                             *'+sLineBreak+
              '*   0 = chqNenhuma,   1 = chqImpressoraECF,  2 = chqImpressoraComum,         *'+sLineBreak+
              '*   3 = chqBematech,  4 = chqChronos,        5 = chqSchalter,                *'+sLineBreak+
              '*   6 = chqPerto,     7 = chqSotomaq,        8 = chqUrano                    *'+sLineBreak+
              '*                                                                            *'+sLineBreak+
              '* Modelos de GAV                                                             *'+sLineBreak+
              '*   0 = gavNenhuma,       1 = gavSerialMenno,    2 = gavSerialGerbo,         *'+sLineBreak+
              '*   3 = gavImpressoraECF, 4 = gavImpressoraComum                             *'+sLineBreak+
              '*                                                                            *'+sLineBreak+
              '*   AberturaAntecipada = ( 0 = aaIgnorar , 1 = aaException, 2 = aaAguardar ) *'+sLineBreak+
              '*                                                                            *'+sLineBreak+
              '* Modelos de DIS                                                             *'+sLineBreak+
              '*   0 = disNenhum,        1 = disGertecSerial,   2 = disGertecTeclado,       *'+sLineBreak+
              '*   3 = disKeytecTeclado                                                     *'+sLineBreak+
              '*                                                                            *'+sLineBreak+
              '* Modelos de BAL                                                             *'+sLineBreak+
              '*   0 = balNenhum,        1 = balFilizola,   2 = balToledo                   *'+sLineBreak+
              '*                                                                            *'+sLineBreak+
              '*                                                                            *'+sLineBreak+
              '******************************************************************************'+sLineBreak+sLineBreak ) ;

     Memo.Add('[ACBrMonitor]'+sLineBreak+
              'Modo_TCP=1'+sLineBreak+
              'Modo_TXT=0'+sLineBreak+
              'TCP_Porta=3434'+sLineBreak+
              'Conexoes_Simultaneas=1'+sLineBreak+
              'TXT_Entrada=ENT.TXT'+sLineBreak+
              'TXT_Saida=SAI.TXT'+sLineBreak+
              'Intervalo=10'+sLineBreak+
              'Protegido=0'+sLineBreak+
              'Senha='+sLineBreak+
              'Gravar_Log=1'+sLineBreak+
              'Arquivo_Log=LOG.TXT'+sLineBreak+
              'Linhas_Log=50000'+sLineBreak+
              'Comandos_Remotos=0'+sLineBreak+sLineBreak ) ;

     Memo.Add('[ECF]'+sLineBreak+
              'Modelo=0'+sLineBreak+
              'Porta=COM1'+sLineBreak+
              'Timeout=5'+sLineBreak+
              'ArredondamentoPorQtd=0'+sLineBreak+
              'DescricaoGrande=0'+sLineBreak+
              'GavetaSinalInvertido=0'+sLineBreak+
              'IntervaloAposComando=100'+sLineBreak+sLineBreak ) ;

     Memo.Add('[CHQ]'+sLineBreak+
              'Modelo=0'+sLineBreak+
              'Porta='+sLineBreak+
              'VerificaFormulario=0'+sLineBreak+
              'Favorecido=Daniel Simoes de Almeida'+sLineBreak+
              'Cidade=Tatui - SP'+sLineBreak+
              'PathBemafiINI='+sLineBreak+sLineBreak ) ;

     Memo.Add('[GAV]'+sLineBreak+
              'Modelo=0'+sLineBreak+
              'Porta='+sLineBreak+
              'StringAbertura=#027,v,#140 | Bematech'+sLineBreak+
              'AberturaIntervalo=300'+sLineBreak+
              'AcaoAberturaAntecipada=2'+sLineBreak+sLineBreak ) ;

     Memo.Add('[DIS]'+sLineBreak+
              'Modelo=0'+sLineBreak+
              'Porta='+sLineBreak+
              'Intervalo=300'+sLineBreak+
              'Passos=1'+sLineBreak+sLineBreak ) ;
              
     Memo.Add('[LCB]'+sLineBreak+
              'Porta='+sLineBreak+
              'Intervalo=100'+sLineBreak+
              'SufixoLeitor=#13'+sLineBreak+
              'ExcluirSufixo=0'+sLineBreak+
              'PrefixoAExcluir='+sLineBreak+
              'SufixoIncluir='+sLineBreak+
              'Dispositivo=/dev/tty'+sLineBreak+
              'Teclado=1'+sLineBreak+
              'Device='+sLineBreak+sLineBreak );

     Memo.Add('[BAL]'+sLineBreak+
              'Modelo=0'+sLineBreak+
              'Porta='+sLineBreak+
              'Intervalo=200'+sLineBreak+sLineBreak ) ;

     Memo.SaveToFile(ExtractFilePath( ParamStr(0) )+ 'ACBrMonitor.ini');
  finally
     Memo.Free ;
  end ;

  Raise Exception.Create( 'Arquivo "ACBrMonitor.ini" não encontrado.'+sLineBreak+
     'Arquivo "ACBrMonitor.ini" foi criado utilizando valores padrões'+sLineBreak+
     'Edite o arquivo "ACBrMonitor.ini" ou execute o programa ACBrMonitor'+sLineBreak+
     'para configura-lo corretamente' ) ;
end;

procedure Tdm.SetDisWorking(const Value: Boolean);
begin
  if ACBrLCB1.Ativo then
     if Value then
      begin
        fsOldIntervaloLCB  := ACBrLCB1.Intervalo ;
        ACBrLCB1.Intervalo := 0 ;
      end
     else
        ACBrLCB1.Intervalo := fsOldIntervaloLCB ;

  fsDisWorking := Value;
end;

procedure Tdm.ACBrLCB1LeCodigo(Sender: TObject);
Var
  Codigo : String ;
  {$IFDEF LINUX}
   fd, I  : Integer ;
   C : Char ;
  {$ENDIF}
begin
  WriteLn('LCB -> '+ACBrLCB1.UltimoCodigo) ;
  if GravarLog then
     WriteToTXT(ArqLogTXT, 'LCB -> '+ACBrLCB1.UltimoCodigo);

  if LCBTeclado then
  begin
     Codigo := ACBrLCB1.UltimoCodigo ;
     if Codigo = '' then
        exit ;

     {$IFDEF MSWINDOWS}
     Codigo := Codigo + LCBSufixoIncluir ;
     SendKeys( pchar(Codigo) , True ) ;
     {$ENDIF}

     { Alguem sabe como enviar as teclas para o Buffer do KDE ??? }
     {$IFDEF LINUX}
     Codigo := Codigo + TraduzComando( LCBSufixoIncluir ) ;
     fd := FileOpen(LCBDispositivo,O_WRONLY + O_NONBLOCK) ;
     if fd < 0 then
        Writeln('Erro ao abrir o dispositivo: '+LCBDispositivo)
     else
        try
           for I := 1 to length(Codigo) do
           begin
              C := Codigo[I] ;
              FpIOCtl(fd, TIOCSTI, @C);
           end ;
        finally
           FileClose(fd);
        end;

//   WriteToTXT('/dev/stdin',Codigo,False);
//   RunCommand('echo','"TESTE'+Codigo+'" > /dev/tty1',true) ;
     {$ENDIF}
  end ;

end;

procedure Tdm.ACBrRFD1GetKeyRSA(var PrivateKey_RSA: String);
begin
  PrivateKey_RSA := LerChaveSWH ;
end;

procedure Tdm.TcpServerConecta(const TCPBlockSocket : TTCPBlockSocket ;
  var Enviar : AnsiString) ;
begin
  Conexao := TCPBlockSocket;
  Resposta( '', 'ACBrMonitorConsole Ver. ' + Versao + sLineBreak +
                'Conectado em: ' + FormatDateTime('dd/mm/yy hh:nn:ss', now) + sLineBreak +
                'Máquina: ' + Conexao.GetRemoteSinIP + sLineBreak +
                'Esperando por comandos.' ) ;
end;

procedure Tdm.TcpServerDesConecta(const TCPBlockSocket : TTCPBlockSocket ;
  Erro : Integer ; ErroDesc : String) ;
begin
  Conexao := TCPBlockSocket;
  WriteLn( 'ALERTA: Fim da Conexão com: ' +
    Conexao.GetRemoteSinIP + ' em: ' +
    FormatDateTime('dd/mm/yy hh:nn:ss', now) ) ;
end;

procedure Tdm.TcpServerRecebeDados(const TCPBlockSocket : TTCPBlockSocket ;
  const Recebido : AnsiString ; var Enviar : AnsiString) ;
Var
  CmdEnviado : String ;
begin
  { Le o que foi enviado atravez da conexao TCP }
  CmdEnviado := trim(Recebido) ;
  if CmdEnviado <> '' then
  begin
     Conexao := TCPBlockSocket;
     ComandosAProcessar.Text := CmdEnviado  ;

     Processar ;
  end ;
end;

end.

