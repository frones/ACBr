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

{$mode objfpc}{$H+}

unit DoACBrUnit ;

interface
Uses Classes, TypInfo, SysUtils, CmdUnit, IniFiles;
{$IFDEF MSWINDOWS}
  function BlockInput (fBlockInput: boolean): dword; stdcall; external 'user32.dll';
{$ENDIF}

Procedure DoACBr( {%H-}Cmd : TACBrCmd ) ;
Procedure VerificaPermiteComandosRemoto ;
Function ConvertStrRecived( AStr: AnsiString ) : AnsiString ;
Function LerConverterIni( AStr: AnsiString ) : TMemIniFile;
Function StringIsXML(AStr: AnsiString) : Boolean;

implementation
Uses ACBrUtil, DateUtils, pcnConversao, utilUnit,
  {$IFDEF MSWINDOWS}sndkey32, Windows,{$ENDIF}
  {$IFNDEF NOGUI}Forms, ACBrMonitor1 {$ELSE}ACBrMonitorConsoleDM {$ENDIF} ;

Procedure DoACBr( Cmd : TACBrCmd ) ;
Var AltTab : Boolean ;
    Memo   : TStringList ;
    dtFim  : TDateTime ;
    {$IFDEF MSWINDOWS}
     nWait  : Integer ;
    {$ENDIF}
    Files  : String ;
    NumAmbiente : integer;
begin
  with {$IFNDEF NOGUI}FrmACBrMonitor{$ELSE}dm{$ENDIF} do
  begin
     try
        if Cmd.Metodo = 'ativo' then
           Cmd.Resposta := 'ATIVO'

        else if Cmd.Metodo = 'run' then
         begin
           VerificaPermiteComandosRemoto ;

           if Cmd.Params(0) = '' then
              raise Exception.Create('Linha de comando não informada');

           AltTab := StrToBoolDef(Cmd.Params(4),False) ; { Envia ALT-TAB quando Terminar ?}
           RunCommand( Cmd.Params(0),                       { Linha de comando }
                       Cmd.Params(1),                       { Parametros adicionais }
                       StrToBoolDef(Cmd.Params(2),False), { Aguarda termino execuçao ? }
                       StrToIntDef(Cmd.Params(3),1));
           {$IFDEF MSWINDOWS}
           if AltTab then
              SendKeys(pchar('%{TAB}'), False);
           {$ENDIF}
         end

        {$IFNDEF NOGUI}
          else if Cmd.Metodo = 'restaurar' then
             Restaurar1Click( FrmACBrMonitor )

          else if Cmd.Metodo = 'ocultar' then
             Ocultar1Click( FrmACBrMonitor )

          else if Cmd.Metodo = 'encerrarmonitor' then
             Application.Terminate
        {$ENDIF}

        {$IFDEF MSWINDOWS}
         else if Cmd.Metodo = 'sendkeys' then
            SendKeys( PChar(Cmd.Params(0)),                  { Teclas a Enviar }
                      StrToBoolDef(Cmd.Params(1),False) )           { Espera ? }
            
         else if Cmd.Metodo = 'appactivate' then
          begin
            nWait := StrToIntDef( Cmd.Params(1), 0 ) ;
            if nWait > 0 then
               Sleep(nWait);
            AppActivate( PChar(Cmd.Params(0)) ) ;
          end

         else if Cmd.Metodo = 'appexists' then
            Cmd.Resposta := BoolToStr( AppExists( PChar(Cmd.Params(0)) ), true )

         else if Cmd.Metodo = 'blockinput' then
           BlockInput( StrToBool(Cmd.Params(0)) )
        {$ENDIF}

        else if Cmd.Metodo = 'savetofile' then
         begin
           VerificaPermiteComandosRemoto ;

           Memo := TStringList.Create ;
           try
              Memo.Clear ;
              Memo.Text := ConvertStrRecived( cmd.Params(1) );
              Memo.SaveToFile( Cmd.Params(0) );
           finally
              Memo.Free ;
           end ;
         end

        else if Cmd.Metodo = 'loadfromfile' then
         begin
           VerificaPermiteComandosRemoto ;

           Files := Cmd.Params(0) ;
           dtFim := IncSecond(now, StrToIntDef(Cmd.Params(1),1) ) ;
           while now <= dtFim do
           begin
              if FileExists( Files ) then
              begin
                 Memo  := TStringList.Create ;
                 try
                    Memo.Clear ;
                    Memo.LoadFromFile( Files ) ;
                    Cmd.Resposta := Memo.Text ;
                    Break ;
                 finally
                    Memo.Free ;
                 end ;
              end ;

              {$IFNDEF NOGUI}
               Application.ProcessMessages ;
              {$ENDIF}
              sleep(100) ;
           end ;

           if not FileExists( Cmd.Params(0) ) then
              raise Exception.Create('Arquivo '+Cmd.Params(0)+' não encontrado')
         end

        else if Cmd.Metodo = 'filesexists' then
         begin
           VerificaPermiteComandosRemoto ;

           Files := Cmd.Params(0) ;
           dtFim := IncSecond(now, StrToIntDef(Cmd.Params(1),0) ) ;
           while (now <= dtFim) and ( not FileExists( Files ) ) do
           begin
              {$IFNDEF NOGUI}
               Application.ProcessMessages ;
              {$ENDIF}
              sleep(100) ;
           end ;

           Cmd.Resposta := BoolToStr(FilesExists( Files ), True) ;
         end

        else if Cmd.Metodo = 'deletefiles' then
         begin
           VerificaPermiteComandosRemoto ;

           Files := Trim(Cmd.Params(0)) ;
           if (Files = '') or (Files = '*') or (Files = '*.*') then
              raise Exception.Create( 'Mascara inválida: ['+Files+']') ;

           DeleteFiles( Files ) ;
           if FilesExists( Files ) then
              raise Exception.Create('Arquivo(s) ['+Files+'] ainda existe(m)') ;
         end

        else if Cmd.Metodo = 'setcertificado' then //NFe.SetCertificado(cCertificado,cSenha)
         begin
           if (Cmd.Params(0)<>'') then
            begin
              if FileExists(Cmd.Params(0)) then
               begin
                 ACBrNFe1.Configuracoes.Certificados.ArquivoPFX   := Cmd.Params(0);
                 ACBrCTe1.Configuracoes.Certificados.ArquivoPFX   := Cmd.Params(0);
                 ACBrMDFe1.Configuracoes.Certificados.ArquivoPFX  := Cmd.Params(0);
                 ACBrGNRE1.Configuracoes.Certificados.ArquivoPFX  := Cmd.Params(0);
                 edtArquivoPFX.Text :=  ACBrNFe1.Configuracoes.Certificados.ArquivoPFX ;
                 edtNumeroSerie.Text :=  '';
               end
              else
               begin
                 ACBrNFe1.Configuracoes.Certificados.NumeroSerie  := Cmd.Params(0);
                 ACBrCTe1.Configuracoes.Certificados.NumeroSerie  := Cmd.Params(0);
                 ACBrMDFe1.Configuracoes.Certificados.NumeroSerie := Cmd.Params(0);
                 ACBrGNRE1.Configuracoes.Certificados.NumeroSerie := Cmd.Params(0);
                 edtNumeroSerie.Text :=  ACBrNFe1.Configuracoes.Certificados.NumeroSerie ;
                 edtArquivoPFX.Text :=  '';
               end;

              ACBrNFe1.Configuracoes.Certificados.Senha  := Cmd.Params(1);
              ACBrCTe1.Configuracoes.Certificados.Senha  := Cmd.Params(1);
              ACBrMDFe1.Configuracoes.Certificados.Senha := Cmd.Params(1);
              ACBrGNRE1.Configuracoes.Certificados.Senha := Cmd.Params(1);
              edtSenha.Text := ACBrNFe1.Configuracoes.Certificados.Senha;
              SalvarIni;
            end
           else
              raise Exception.Create('Certificado '+Cmd.Params(0)+' Inválido.');
         end

        else if Cmd.Metodo = 'setwebservice' then
        begin
          if Cmd.Params(0) <> '' then
          begin
            ACBrNFe1.Configuracoes.WebServices.UF  := Cmd.Params(0);
            ACBrCTe1.Configuracoes.WebServices.UF  := Cmd.Params(0);
            ACBrMDFe1.Configuracoes.WebServices.UF := Cmd.Params(0);
            ACBrGNRE1.Configuracoes.WebServices.UF := Cmd.Params(0);
            cbUF.Text := Cmd.Params(0);
          end;

          if Cmd.Params(1) <> '' then
          begin
            NumAmbiente := StrToIntDef(Cmd.Params(1), 2);

            if (NumAmbiente < 1) or (NumAmbiente > 2) then
              raise Exception.Create('Ambiente Inválido: ' + IntToStr(NumAmbiente));

            ACBrNFe1.Configuracoes.WebServices.Ambiente  := TpcnTipoAmbiente(NumAmbiente);
            ACBrCTe1.Configuracoes.WebServices.Ambiente  := TpcnTipoAmbiente(NumAmbiente);
            ACBrMDFe1.Configuracoes.WebServices.Ambiente := TpcnTipoAmbiente(NumAmbiente);
            ACBrGNRE1.Configuracoes.WebServices.Ambiente := TpcnTipoAmbiente(NumAmbiente);
            rgTipoAmb.ItemIndex                          := NumAmbiente - 1;
          end;
          SalvarIni;

          cmd.Resposta := Format('WebService configurado %s',[Cmd.Params(0)]);
	end

        else if Cmd.Metodo ='datahora' then
           Cmd.Resposta := FormatDateTime('dd/mm/yyyy hh:nn:ss', Now )
					 
        else if Cmd.Metodo ='data' then
           Cmd.Resposta := FormatDateTime('dd/mm/yyyy', Now )
					 
        else if Cmd.Metodo ='hora' then
           Cmd.Resposta := FormatDateTime('hh:nn:ss', Now ) 

        else if pos('|'+Cmd.Metodo+'|', '|exit|bye|fim|sair|') > 0 then {fecha conexao}
         begin
           Cmd.Resposta := 'Obrigado por usar o ACBrMonitor' ;
           {$IFNDEF NOGUI}
             mCmd.Lines.Clear;
           {$ELSE}
             WriteLn( 'Obrigado por usar o ACBrMonitorConsole' ) ;
           {$ENDIF}

           if Assigned( Conexao ) then
              Conexao.CloseSocket ;
         end

         else if Cmd.Metodo = 'lerini' then //ACBR.LerIni  Recarrega configurações do arquivo INI
           LerIni

         else if Cmd.Metodo = 'ajustalinhaslog' then //ACBR.AjustaLinhasLog
           AjustaLinhasLog

         else if cmd.Metodo = 'versao' then
            Cmd.Resposta := VersaoACBr

        ELSE
           raise Exception.Create('Comando inválido ('+ copy(Cmd.Comando,6,length(Cmd.Comando))+')') ;

     finally
        { Nada a fazer aqui por enquanto... :) }
     end ;
  end ;
end ;

Procedure VerificaPermiteComandosRemoto ;
begin
  with {$IFNDEF NOGUI}FrmACBrMonitor{$ELSE}dm{$ENDIF} do
  begin
     {$IFNDEF NOGUI}
       if not cbComandos.Checked then
     {$ELSE}
       if not PermiteComandos then
     {$ENDIF}
          raise Exception.Create('Comandos Remotos não são permitidos');
  end ;
end ;

Function ConvertStrRecived( AStr: AnsiString ) : AnsiString ;
 Var P   : Integer ;
     Hex : String ;
     CharHex : Char ;
begin
  { Verificando por codigos em Hexa }
  Result := AStr ;

  P := pos('\x',Result) ;
  while P > 0 do
  begin
     Hex := copy(Result,P+2,2) ;

     try
        CharHex := Chr(StrToInt('$'+Hex)) ;
     except
        CharHex := ' ' ;
     end ;

     Result := StringReplace(Result,'\x'+Hex,CharHex,[rfReplaceAll]) ;
     P      := pos('\x',Result) ;
  end ;
end ;

function LerConverterIni(AStr: AnsiString): TMemIniFile;
var
  SL: TStringList;
begin
  Result := TMemIniFile.Create(' ');
  SL     := TStringList.Create;
  try
    try
      if (pos(#10,aStr) = 0) and FilesExists(Astr) then
        SL.LoadFromFile(AStr)
      else
        SL.Text := ConvertStrRecived( Astr );

      Result.SetStrings(SL);
    except
      on E: Exception do
      begin
        raise Exception.Create('Erro ao carregar arquivo'+sLineBreak+E.Message);
      end;
    end;
  finally
    SL.Free;
  end;
end;

function StringIsXML(AStr: AnsiString): Boolean;
begin
  Result :=(pos('<', AStr) > 0) and (pos('>', AStr) > 0);
end;

end.

