{******************************************************************************}
{ Projeto: ACBr Monitor                                                        }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para }
{ criar uma interface de comunicação com equipamentos de automacao comercial.  }
{                                                                              }
{ Direitos Autorais Reservados (c) 2010 Daniel Simões de Almeida               }
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
{       Rua Coronel Aureliano de Camargo, 973 - Tatuí - SP - 18270-170         }
{                                                                              }
{******************************************************************************}

program ACBrMonitorConsole;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
   unix, baseunix, termio,
   {$IFDEF UseCThreads}cthreads,{$ENDIF}
  {$ENDIF}
  Classes, SysUtils, IniFiles, CustApp, ACBrBase, ACBrUtil, ACBrConsts, ACBrECF,
  ACBrECFNaoFiscal, UtilUnit, ACBrMonitorConsoleDM, DoACBrUnit, DoETQUnit,
  DoLCBUnit, DoBALUnit, DoCEPUnit, DoCHQUnit, DoDISUnit, DoECFUnit, DoGAVUnit,
  DoIBGEUnit, DoECFBemafi32, DoECFObserver, DoEmailUnit ; // Units de tradução dos comandos

type

  { TACBrMonitorConsole }

  TACBrMonitorConsole = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TACBrMonitorConsole }

Var I : Integer;
    ArqINI : String ;
    Ini    : TIniFile ;

{$IFDEF LINUX}
procedure SignalProc(SigNum: Integer); cdecl;
 Var LogMsg : String ;
begin
  LogMsg := 'Sinal Interceptado: ' ;

  case SigNum of
     SIGINT  :
        LogMsg := LogMsg + 'SIGINT' ;
     SIGSTOP :
        LogMsg := LogMsg + 'SIGSTOP' ;
     SIGTSTP :
        LogMsg := LogMsg + 'SIGTSTP' ;
     SIGQUIT :
        LogMsg := LogMsg + 'SIGQUIT' ;
  else
     LogMsg := LogMsg + IntToStr(SigNum) ;
  end;

  WriteLn( LogMsg );
  if dm.GravarLog then
      WriteToTXT(dm.ArqLogTXT, LogMsg );
end;
{$ENDIF}

procedure TACBrMonitorConsole.DoRun;
var
  ErrorMsg: String;
  Txt : String ;
  IpList, Linhas: TStringList;
  MS: TMemoryStream;
  S : AnsiString;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  {$IFDEF LINUX}
   FpSignal(SIGINT , @SignalProc); // catch the signal SIGINT  to procedure SignalProc
   FpSignal(SIGSTOP, @SignalProc); // catch the signal SIGSTOP to procedure SignalProc
   FpSignal(SIGTSTP, @SignalProc); // catch the signal SIGTSTP to procedure SignalProc
   FpSignal(SIGQUIT, @SignalProc); // catch the signal SIGQUIT to procedure SignalProc

   FpUmask( 0 ) ;
  {$ENDIF}

  { Definindo constantes de Verdadeiro para TrueBoolsStrs }
  SetLength( TrueBoolStrs, 5 ) ;
  TrueBoolStrs[0] := 'True' ;
  TrueBoolStrs[1] := 'T' ;
  TrueBoolStrs[2] := 'Verdadeiro' ;
  TrueBoolStrs[3] := 'V' ;
  TrueBoolStrs[4] := 'Ok' ;

  { Definindo constantes de Falso para FalseBoolStrs }
  SetLength(FalseBoolStrs, 3);
  FalseBoolStrs[0] := 'False' ;
  FalseBoolStrs[1] := 'F' ;
  FalseBoolStrs[2] := 'Falso' ;

  { ---------------- Inicializando as variaveis ---------------- }
  TRY
    writeln('Bem vindo ao ACBrMonitorConsole '+Versao+' - ACBr: '+ACBR_VERSAO ) ;
    writeln('') ;

    { Lendo o arquivo INI. Se houver erro, dispara exception }
    dm.LerIni ;

    { Ajustando o tamanho do arquivo de LOG }
    try
       dm.AjustaLinhasLog ;
    except
       on E : Exception do
          WriteLn( E.Message ) ;
    end ;

    { Ativando comunicacao TCP/IP }
    dm.TcpServer.Ativo := dm.IsTCP ;
    WriteLn('ACBrMonitorConsole - Ver.'+Versao);
    WriteLn('Aguardando comandos ACBr');

     { Exibindo estado atual e finalizando a inicializacao }
     try
        if dm.IsTCP then
         begin
           if dm.TcpServer.Ativo then
           begin
              Txt := 'Endereço Local: ['+dm.TcpServer.TCPBlockSocket.LocalName + '],   IP: ';
              with dm.TcpServer.TCPBlockSocket do
              begin
                 IpList := TStringList.Create;
                 try
                    ResolveNameToIP( LocalName, IpList);
                    For I := 0 to IpList.Count-1 do
                       if pos(':',IpList[I]) = 0 then
                          Txt := Txt + '   '+IpList[I] ;
                 finally
                    IpList.Free;
                 end ;
              end ;

              WriteLn( Txt );
              WriteLn( 'Porta: [' + dm.TcpServer.Port + ']') ;
           end ;
         end
        else
         begin
           WriteLn( 'Monitorando TXT em: '+dm.ArqEntTXT);
           WriteLn( 'Respostas gravadas em:'+dm.ArqSaiTXT);
         end ;

        if dm.GravarLog then
           WriteLn( 'Log de comandos será gravado em: '+dm.ArqLogTXT) ;

        { Se for NAO fiscal, desliga o AVISO antes de ativar }
        if dm.ACBrECF1.Modelo = ecfNaoFiscal then
        begin
           ArqIni := (dm.ACBrECF1.ECF as TACBrECFNaoFiscal).NomeArqINI ;
           if FileExists( ArqIni ) then
           begin
              Ini := TIniFile.Create( ArqIni ) ;
              try
                Ini.WriteString('Variaveis','Aviso_Legal','NAO');
             finally
                Ini.Free ;
             end ;
           end ;
        end ;
     except
        on E : Exception do
           WriteLn( E.Message ) ;
     end ;

     { Loop INFINITO, esperando arquivo TXT }
     repeat
        if FileExists( dm.ArqEntTXT ) then  { Existe arquivo para ler ? }
        begin
          try
            Linhas := TStringList.Create;

            dm.TipoCMD := 'A' ;
            if ( UpperCase(ExtractFileName( dm.ArqEntTXT )) = 'BEMAFI32.CMD' ) then
              dm.TipoCMD := 'B'
            else if ( UpperCase(ExtractFileName( dm.ArqEntTXT )) = 'DARUMA.CMD' ) then
              dm.TipoCMD := 'D' ;

            { Lendo em MemoryStream temporário para nao apagar comandos nao processados }
            MS := TMemoryStream.Create;
            try
              MS.LoadFromFile(dm.ArqEntTXT);
              MS.Position := 0;
              SetLength(S, MS.Size);
              MS.ReadBuffer(PChar(S)^, MS.Size);
              if dm.ConvENT then
                S := AnsiToUtf8(S);
              Linhas.Text := S;
            finally
              MS.Free;
            end;

            TryDeleteFile(dm.ArqEntTXT, 1000); // Tenta apagar por até 1 segundo

            if dm.TipoCMD = 'B' then
              Linhas.Text := TraduzBemafi( Linhas.Text )
            else if dm.TipoCMD = 'D' then
              Linhas.Text := TraduzObserver( Linhas.Text );

            dm.ComandosAProcessar.AddStrings( Linhas );
          finally
            Linhas.Free ;
          end ;

          dm.Processar ;
        end;

        Sleep( dm.Intervalo ) ;
     until False ;
  EXCEPT
    On E : Exception do
    begin
       if dm.GravarLog then
          WriteToTXT(dm.ArqLogTXT, 'Exception: ' + E.Message);

       raise ;
    end ;
  END ;

  // stop program loop
  Terminate;
end;

constructor TACBrMonitorConsole.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  dm := Tdm.Create(Self);
end;

destructor TACBrMonitorConsole.Destroy;
begin
  dm.ACBrECF1.Desativar ;
  dm.ACBrCHQ1.Desativar ;
  dm.ACBrGAV1.Desativar ;
  dm.ACBrDIS1.Desativar ;
  dm.ACBrLCB1.Desativar ;

  dm.TcpServer.OnDesConecta := nil ;
  dm.TCPServer.Ativo        := False ;    { Desliga TCP }

  dm.Free ;

  inherited Destroy;
end;

procedure TACBrMonitorConsole.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TACBrMonitorConsole;

begin
  Application:=TACBrMonitorConsole.Create(nil);
  try
     Application.Run;
  finally
     Application.Free;
  end ;
end.

