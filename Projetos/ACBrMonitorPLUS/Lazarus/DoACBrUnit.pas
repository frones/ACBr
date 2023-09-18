{*******************************************************************************}
{ Projeto: ACBrMonitor                                                          }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para  }
{ criar uma interface de comunicação com equipamentos de automacao comercial.   }
{                                                                               }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida                }
{                                                                               }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr       }
{                                                                               }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la  }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela   }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério)  }
{ qualquer versão posterior.                                                    }
{                                                                               }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU       }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)               }
{                                                                               }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,   }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.           }
{ Você também pode obter uma copia da licença em:                               }
{ http://www.opensource.org/licenses/gpl-license.php                            }
{                                                                               }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br }
{        Rua Cel.Aureliano de Camargo, 963 - Tatuí - SP - 18270-170             }
{                                                                               }
{*******************************************************************************}

{$mode objfpc}{$H+}

unit DoACBrUnit ;

interface
Uses
  Classes, TypInfo, SysUtils, CmdUnit, synautil, synacode, ACBrMonitorConsts, ACBrLibCertUtils,
  ACBrMonitorConfig, pcnAuxiliar;

{$IFDEF MSWINDOWS}
  function BlockInput (fBlockInput: boolean): dword; stdcall; external 'user32.dll';
{$ENDIF}

type

{ TACBrObjetoACBr}
TACBrObjetoACBr = class(TACBrObjetoDFe)
private
public
  constructor Create(AConfig: TMonitorConfig); reintroduce;
  procedure Executar(ACmd: TACBrCmd); override;

  Procedure EncerrarMonitor;
  Procedure VerificaPermiteComandosRemoto ;

end;

{ TMetodoAtivo }

TMetodoAtivo = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoRun }

TMetodoRun = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoRestaurar }

TMetodoRestaurar = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoOcultar }

TMetodoOcultar = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEncerrarmonitor }

TMetodoEncerrarmonitor = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSendKeys }

TMetodoSendKeys = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAppActivate }

TMetodoAppActivate = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAppExists }

TMetodoAppExists = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoBlockInput }

TMetodoBlockInput = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSavetofile }

TMetodoSavetofile = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoLoadfromfile }

TMetodoLoadfromfile = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoFileExists }

TMetodoFileExists = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCopyFile }

TMetodoCopyFile = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoDeleteFiles }

TMetodoDeleteFiles = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetcertificado }

TMetodoSetcertificado = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetTimeZone }

TMetodoSetTimeZone = class(TACBrMetodo)
public
  procedure Executar; override;
end;


{ TMetodoSetWebservice }

TMetodoSetWebservice = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoObterCertificados }

TMetodoObterCertificados = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoLerini }

TMetodoLerini = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAjustaLnhasLog }

TMetodoAjustaLnhasLog = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEncodeBase64 }

TMetodoEncodeBase64 = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoDecodeBase64 }

TMetodoDecodeBase64= class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoRoundABNT}

TMetodoRoundABNT= class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoVersao }

TMetodoVersao = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoDatahora }

TMetodoDatahora = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoData }

TMetodoData = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoHora }

TMetodoHora = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoExit }

TMetodoExit = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoBye }

TMetodoBye = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSair }

TMetodoSair = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoFim }

TMetodoFim = class(TACBrMetodo)
public
  procedure Executar; override;
end;


implementation
Uses ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Math, DateUtils, pcnConversao, utilUnit,
  {$IFDEF MSWINDOWS}sndkey32, Windows,{$ENDIF}
  {$IFNDEF NOGUI}Forms, ACBrMonitor1 {$ELSE}ACBrMonitorConsoleDM {$ENDIF},
  FileUtil, ACBrDFeSSL;

{ TMetodoFim }

procedure TMetodoFim.Executar;
begin
  with TACBrObjetoACBr(fpObjetoDono) do
    EncerrarMonitor;

end;

{ TMetodoSair }

procedure TMetodoSair.Executar;
begin
  with TACBrObjetoACBr(fpObjetoDono) do
    EncerrarMonitor;

end;

{ TMetodoBye }

procedure TMetodoBye.Executar;
begin
  with TACBrObjetoACBr(fpObjetoDono) do
    EncerrarMonitor;

end;

{ TMetodoExit }

procedure TMetodoExit.Executar;
begin
  with TACBrObjetoACBr(fpObjetoDono) do
    EncerrarMonitor;

end;

{ TMetodoHora }

procedure TMetodoHora.Executar;
begin
  with TACBrObjetoACBr(fpObjetoDono) do
    fpCmd.Resposta := FormatDateTime('hh:nn:ss', Now )
end;

{ TMetodoData }

procedure TMetodoData.Executar;
begin
  with TACBrObjetoACBr(fpObjetoDono) do
    fpCmd.Resposta := FormatDateTime('dd/mm/yyyy', Now )
end;

{ TMetodoDatahora }

procedure TMetodoDatahora.Executar;
begin
  with TACBrObjetoACBr(fpObjetoDono) do
    fpCmd.Resposta := FormatDateTime('dd/mm/yyyy hh:nn:ss', Now )
end;

{ TMetodoVersao }

procedure TMetodoVersao.Executar;
begin
  with TACBrObjetoACBr(fpObjetoDono) do
    fpCmd.Resposta := VersaoACBr
end;

{ TMetodoRoundABNT }

{ Params: 0 - Double: Valor para arredondamento
          1 - integer: Digitos de Decimais p/ Arredondamento}
procedure TMetodoRoundABNT.Executar;
var
   AValue: Double;
   ADigitos: Integer;
begin
  AValue := StrToFloatDef(fpCmd.Params(0),0);
  ADigitos := StrToIntDef(fpCmd.Params(1),2);

  with TACBrObjetoACBr(fpObjetoDono) do
    fpcmd.Resposta := FloatToStr(RoundABNT( AValue, ADigitos));
end;

{ TMetodoEncodeBase64 }

{ Params: 0 - String: Path Arquivo a ser transferido
          1 - integer: TimeOut para Leitura em milissegundos}
procedure TMetodoEncodeBase64.Executar;
var
  AOrigem: String;
  ATime: Integer;
  dtFim: TDateTime;

  AStream: TMemoryStream;
begin
  AOrigem := trim(fpCmd.Params(0));
  ATime := StrToIntDef(fpCmd.Params(1),1);

  with TACBrObjetoACBr(fpObjetoDono) do
  begin
    VerificaPermiteComandosRemoto;

    dtFim := IncSecond(now, ATime ) ;
    while now <= dtFim do
    begin
      if FileExists(AOrigem) then
      begin
        AStream:= TMemoryStream.Create;
        try
          AStream.LoadFromFile(AOrigem);
          AStream.Position:= 0;
          fpcmd.Resposta := EncodeBase64( ReadStrFromStream(AStream, AStream.Size) );
          Break;

        finally
          AStream.Free;
        end;

      end
      else
        raise Exception.Create('Arquivo '+AOrigem+' não existe.');

      sleep(100) ;

    end;
  end;

end;

{ TMetodoDecodeBase64 }

{ Params: 0 -  String: Conteudo Base64 a ser Decodificado
          1 -  String: Path e nome Arquivo a ser gravado}
procedure TMetodoDecodeBase64.Executar;
var
  ABase64: Ansistring;
  ADestino: String;

  AStream: TMemoryStream;
begin
  ABase64 := trim(fpCmd.Params(0));
  ADestino := trim(fpCmd.Params(1));

  if EstaVazio(ABase64) then
    Raise Exception.Create('Informe Arquivo em Base64');

  if EstaVazio(ADestino) then
    Raise Exception.Create('Informe o Path do Arquivo para gravação');

  with TACBrObjetoACBr(fpObjetoDono) do
  begin
    if not (DirectoryExists(ExtractFileDir(ADestino)) ) then
      if not ForceDirectories(ExtractFileDir(ADestino)) then
        raise Exception.Create(' Falha ao criar o Diretório de destino: ' + ExtractFileDir(ADestino));

    AStream:= TMemoryStream.Create;
    try
      AStream.Position:= 0;
      WriteStrToStream(AStream, DecodeBase64(ABase64) );
      AStream.SaveToFile(ADestino);

      fpcmd.Resposta := ADestino;
    finally
      AStream.Free;
    end;

  end;

end;

{ TMetodoAjustaLnhasLog }

procedure TMetodoAjustaLnhasLog.Executar;
begin
  with TACBrObjetoACBr(fpObjetoDono) do
    FrmACBrMonitor.AjustaLinhasLog;
end;

{ TMetodoLerini }

procedure TMetodoLerini.Executar;
begin
  with TACBrObjetoACBr(fpObjetoDono) do
    FrmACBrMonitor.LerIni();
end;

{ TMetodoObterCertificados }

procedure TMetodoObterCertificados.Executar;
begin
  with TACBrObjetoACBr(fpObjetoDono) do
      fpcmd.Resposta := ObterCerticados(frmACBrMonitor.ACBrNFe1.SSL)

end;

{ TMetodoSetWebservice }

{ Params: 0 - String: UF para atualização
          1 - Integer: Código do Ambiente   1-Producao  2-Homologação}
procedure TMetodoSetWebservice.Executar;
var
  AUF: String;
  AAmbiente: Integer;
begin
  AUF:= fpCmd.Params(0);
  AAmbiente:= StrToIntDef(fpCmd.Params(1),2);

  with TACBrObjetoACBr(fpObjetoDono) do
  begin
    if AUF <> '' then
    begin
      FrmACBrMonitor.ACBrNFe1.Configuracoes.WebServices.UF  := AUF;
      FrmACBrMonitor.ACBrCTe1.Configuracoes.WebServices.UF  := AUF;
      FrmACBrMonitor.ACBrMDFe1.Configuracoes.WebServices.UF := AUF;
      FrmACBrMonitor.ACBrGNRE1.Configuracoes.WebServices.UF := AUF;
      FrmACBrMonitor.cbUF.Text := AUF;
    end;

    if fpCmd.Params(1) <> '' then
    begin
      if (AAmbiente < 1) or (AAmbiente > 2) then
        raise Exception.Create('Ambiente Inválido: ' + IntToStr(AAmbiente));

      FrmACBrMonitor.ACBrNFe1.Configuracoes.WebServices.Ambiente  := TpcnTipoAmbiente(AAmbiente);
      FrmACBrMonitor.ACBrCTe1.Configuracoes.WebServices.Ambiente  := TpcnTipoAmbiente(AAmbiente);
      FrmACBrMonitor.ACBrMDFe1.Configuracoes.WebServices.Ambiente := TpcnTipoAmbiente(AAmbiente);
      FrmACBrMonitor.ACBrGNRE1.Configuracoes.WebServices.Ambiente := TpcnTipoAmbiente(AAmbiente);
      FrmACBrMonitor.rgTipoAmb.ItemIndex                          := AAmbiente - 1;
    end;
    FrmACBrMonitor.SalvarIni;

    fpcmd.Resposta := Format('WebService configurado %s',[AUF]);

  end;
end;

{ TMetodoSetcertificado }

{ Params: 0 - String: Path do Arquivo PFX ou Numero de Série
          1 - String: Senha do Certificado}
procedure TMetodoSetcertificado.Executar;
var
  APath: String;
  ASenha: String;
begin
  APath := fpCmd.Params(0);
  ASenha := fpCmd.Params(1);

  with TACBrObjetoACBr(fpObjetoDono) do
  begin
    if (APath <> '') then
    begin
      if FileExists(APath) then
       begin
         FrmACBrMonitor.ACBrNFe1.Configuracoes.Certificados.ArquivoPFX   := APath;
         FrmACBrMonitor.ACBrCTe1.Configuracoes.Certificados.ArquivoPFX   := APath;
         FrmACBrMonitor.ACBrMDFe1.Configuracoes.Certificados.ArquivoPFX  := APath;
         FrmACBrMonitor.ACBrGNRE1.Configuracoes.Certificados.ArquivoPFX  := APath;
         FrmACBrMonitor.edtArquivoPFX.Text :=  FrmACBrMonitor.ACBrNFe1.Configuracoes.Certificados.ArquivoPFX ;
         FrmACBrMonitor.edtNumeroSerie.Text :=  '';
       end
      else
       begin
         FrmACBrMonitor.ACBrNFe1.Configuracoes.Certificados.NumeroSerie  := APath;
         FrmACBrMonitor.ACBrCTe1.Configuracoes.Certificados.NumeroSerie  := APath;
         FrmACBrMonitor.ACBrMDFe1.Configuracoes.Certificados.NumeroSerie := APath;
         FrmACBrMonitor.ACBrGNRE1.Configuracoes.Certificados.NumeroSerie := APath;
         FrmACBrMonitor.edtNumeroSerie.Text :=  FrmACBrMonitor.ACBrNFe1.Configuracoes.Certificados.NumeroSerie ;
         FrmACBrMonitor.edtArquivoPFX.Text :=  '';
       end;

      FrmACBrMonitor.ACBrNFe1.Configuracoes.Certificados.Senha  := ASenha;
      FrmACBrMonitor.ACBrCTe1.Configuracoes.Certificados.Senha  := ASenha;
      FrmACBrMonitor.ACBrMDFe1.Configuracoes.Certificados.Senha := ASenha;
      FrmACBrMonitor.ACBrGNRE1.Configuracoes.Certificados.Senha := ASenha;
      FrmACBrMonitor.edtSenha.Text := FrmACBrMonitor.ACBrNFe1.Configuracoes.Certificados.Senha;
      FrmACBrMonitor.SalvarIni;
    end
    else
      raise Exception.Create('Certificado '+APath+' Inválido.');

  end;

end;

{ TMetodoSetTimeZone }

{ Params: 0 - Integer: Sistema (0,1,2)
          1 - String: Timezone ex -03:00}
procedure TMetodoSetTimeZone.Executar;
var
  ITipoTimeZone: Integer;
  ATimeZone: String;
begin
  ITipoTimeZone  := StrToIntDef(fpCmd.Params(0),0);
  ATimeZone      := fpCmd.Params(1);

  with TACBrObjetoACBr(fpObjetoDono) do
  begin
    case ITipoTimeZone of
      0,1 :
        begin
        FrmACBrMonitor.cbxTimeZoneMode.ItemIndex := ITipoTimeZone;
        FrmACBrMonitor.ACBrNFe1.Configuracoes.WebServices.TimeZoneConf.TimeZoneStr:='';
        FrmACBrMonitor.edTimeZoneStr.Caption := '';
        FrmACBrMonitor.cbxTimeZoneMode.OnChange(FrmACBrMonitor.cbxTimeZoneMode);
        FrmACBrMonitor.SalvarIni;
        end;
      2:
        begin
          if ATimeZone <> '' then
             begin
               FrmACBrMonitor.cbxTimeZoneMode.ItemIndex := ITipoTimeZone;
               FrmACBrMonitor.edTimeZoneStr.Caption := ATimeZone;
               FrmACBrMonitor.ACBrNFe1.Configuracoes.WebServices.TimeZoneConf.TimeZoneStr:=ATimeZone;
               FrmACBrMonitor.cbxTimeZoneModeChange(FrmACBrMonitor.cbxTimeZoneMode);
               FrmACBrMonitor.SalvarIni;
             end
          else
             raise Exception.Create('Informe o parametro ATimeZone Exemplo: -03:00')
        end;
    else
       raise Exception.Create('Parametro inválido !') ;
    end;
  end;

end;

{ TMetodoDeleteFiles }

{ Params: 0 - String: Nome completo do Arquivo a ser Apagado, ou Mascara com Arquivos a serem apagados  }
procedure TMetodoDeleteFiles.Executar;
var
  ANomeArq: String;
begin
  ANomeArq := Trim(fpCmd.Params(0)) ;

  with TACBrObjetoACBr(fpObjetoDono) do
  begin
    VerificaPermiteComandosRemoto ;

    if (ANomeArq = '') or (ANomeArq = '*') or (ANomeArq = '*.*') then
       raise Exception.Create( 'Mascara inválida: ['+ANomeArq+']') ;

    DeleteFiles( ANomeArq ) ;
    if FilesExists( ANomeArq ) then
       raise Exception.Create('Arquivo(s) ['+ANomeArq+'] ainda existe(m)') ;

  end;

end;

{ TMetodoCopyFile }

{ Params: 0 - String: Path do Arquivo de Origem.
  Params: 1 - String: Path do Arquivo de Destino
  params: 3 - Boolean: Permite sobreescrever ou não o arquivo
  params: 4 - Boolean: Permite cirar a estrutura de pastas se nao existir}
procedure TMetodoCopyFile.Executar;
var
  AOrigem: String;
  ADestino: String;
  ASobreencrever: Boolean;
  ACriarDiretorios: Boolean;

begin
  AOrigem := trim(fpCmd.Params(0));
  ADestino := trim(fpcmd.Params(1));
  ASobreencrever := StrToBoolDef(fpcmd.Params(2), False);
  ACriarDiretorios := StrToBoolDef(fpcmd.Params(3), False);

  with TACBrObjetoACBr(fpObjetoDono) do
  begin
    VerificaPermiteComandosRemoto ;
    if FileExists(AOrigem) then
    begin
      if ( not FileExists( ADestino ) ) or ( ASobreencrever ) then
      begin
        if ACriarDiretorios then
          if not ForceDirectories(ExtractFileDir(ADestino)) then
            raise Exception.Create(' Falha ao criar a(s) pasta(s) de destino: ' + ExtractFileDir(ADestino));

        if not CopyFile(AOrigem, ADestino, False) then
          raise Exception.Create( 'Falha ao copiar o arquivo ' + AOrigem + ' para ' + ADestino );
      end;
    end
    else
      raise Exception.Create('Arquivo '+AOrigem+' não existe.');

  end;

end;

{ TMetodoFileExists }

{ Params: 0 - String: Mascara do Arquivo a ser verificado.
  Params: 1 - Integer:  Tempo de Espera, em Milissegundos, até o arquivo ser encontrado ou liberado para Leitura}
procedure TMetodoFileExists.Executar;
var
  ANomeArquivo: String;
  ATempo: Integer;
  dtFim: TDateTime;
begin
  ANomeArquivo := fpCmd.Params(0);
  ATempo := StrToIntDef(fpCmd.Params(1),0);

  with TACBrObjetoACBr(fpObjetoDono) do
  begin
    VerificaPermiteComandosRemoto ;

    dtFim := IncSecond(now, ATempo ) ;
    while (now <= dtFim) and ( not FileExists( ANomeArquivo ) ) do
    begin
       {$IFNDEF NOGUI}
        Application.ProcessMessages ;
       {$ENDIF}
       sleep(100) ;
    end ;

    fpCmd.Resposta := BoolToStr(FilesExists( ANomeArquivo ), True) ;

  end;
end;

{ TMetodoLoadfromfile }

{ Params: 0 - String: Nome do Arquivo a ser lido.
  Params: 1 - Integer:  Tempo de Espera, em Milissegundos, até o arquivo ser encontrado ou liberado para Leitura}
procedure TMetodoLoadfromfile.Executar;
var
  ANomeArquivo: String;
  ATempo: Integer;
  dtFim: TDateTime;
  AMemo: TStringList;
begin
  ANomeArquivo := fpCmd.Params(0);
  ATempo := StrToIntDef(fpCmd.Params(1),1);

  with TACBrObjetoACBr(fpObjetoDono) do
  begin
    VerificaPermiteComandosRemoto ;
    dtFim := IncSecond(now, ATempo ) ;
    while now <= dtFim do
    begin
       if FileExists( ANomeArquivo ) then
       begin
         AMemo  := TStringList.Create ;
         try
            AMemo.Clear ;
            AMemo.LoadFromFile( ANomeArquivo ) ;
            fpCmd.Resposta := AMemo.Text ;
            Break ;
         finally
            AMemo.Free ;
         end ;
       end
       else
         raise Exception.Create('Arquivo '+ANomeArquivo+' não encontrado');

       {$IFNDEF NOGUI}
        Application.ProcessMessages ;
       {$ENDIF}
       sleep(100) ;

    end ;

  end;
end;

{ TMetodoSavetofile }

{ Params: 0 - String: Nome do Arquivo.
  Params: 1 - String: Conteúdo do Arquivo a ser salvo}
procedure TMetodoSavetofile.Executar;
var
  ANomeArq: String;
  AContArq: String;
  AMemo: TStringList;
begin
  ANomeArq := fpCmd.Params(0);
  AContArq := fpCmd.Params(1);

  with TACBrObjetoACBr(fpObjetoDono) do
  begin
    VerificaPermiteComandosRemoto ;

    AMemo := TStringList.Create ;
    try
       AMemo.Clear ;
       AMemo.Text := ConvertStrRecived( AContArq );
       AMemo.SaveToFile( ANomeArq);
    finally
       AMemo.Free ;
    end ;

  end;
end;

{ TMetodoBlockInput }

{ Params: 0 - Boolean: Bloqueia e Desbloqueia o teclado.}
procedure TMetodoBlockInput.Executar;
var
  ABloq: Boolean;
begin
  ABloq := StrToBool(fpCmd.Params(0));

  with TACBrObjetoACBr(fpObjetoDono) do
  begin
    {$IFDEF MSWINDOWS}
      BlockInput( ABloq );
    {$ENDIF}
  end;
end;

{ TMetodoAppExists }

{ Params: 0 - String: Nome da Janela que deseja validar se está em execussão.}
procedure TMetodoAppExists.Executar;
var
  ANomeJanela: String;
begin
  ANomeJanela :=  fpCmd.Params(0);

  with TACBrObjetoACBr(fpObjetoDono) do
  begin
    {$IFDEF MSWINDOWS}
      fpCmd.Resposta := BoolToStr( AppExists( PChar(ANomeJanela) ), true );
    {$ENDIF}
  end;
end;

{ TMetodoAppActivate }

{ Params: 0 - String: Nome da Janela que deseja ajustar o foco.
  Params: 1 - Integer:  (opcional) Tempo em Milissegundos a esperar antes de efetuar o comando.}
procedure TMetodoAppActivate.Executar;
var
  ANomeJanela: String;
  AWait: Integer;
begin
  ANomeJanela :=  fpCmd.Params(0);
  AWait := StrToIntDef( fpCmd.Params(1), 0 );

  with TACBrObjetoACBr(fpObjetoDono) do
  begin
    {$IFDEF MSWINDOWS}
      if AWait > 0 then
        Sleep(AWait);
      AppActivate( PChar(ANomeJanela) ) ;
    {$ENDIF}

  end;
end;

{ TMetodoSendKeys }

{ Params: 0 - String: Teclas a serem enviadas, usando a sintaxe especificada
  Params: 1 - Boolean: Aguardar término do envio das teclas }
procedure TMetodoSendKeys.Executar;
var
  ATeclas: String;
  AEspera: Boolean;
begin
  ATeclas := fpCmd.Params(0);
  AEspera := StrToBoolDef(fpCmd.Params(1),False);

  with TACBrObjetoACBr(fpObjetoDono) do
  begin
    {$IFDEF MSWINDOWS}
      SendKeys( PChar(ATeclas),                  { Teclas a Enviar }
      AEspera )                                  { Espera ? }
    {$ENDIF}

  end;
end;

{ TMetodoEncerrarmonitor }

procedure TMetodoEncerrarmonitor.Executar;
begin
  with TACBrObjetoACBr(fpObjetoDono) do
  begin
    FrmACBrMonitor.Encerrar1Click( FrmACBrMonitor );

  end;
end;

{ TMetodoOcultar }

procedure TMetodoOcultar.Executar;
begin
  with TACBrObjetoACBr(fpObjetoDono) do
  begin
    FrmACBrMonitor.Ocultar1Click( FrmACBrMonitor );

  end;
end;

{ TMetodoRestaurar }

procedure TMetodoRestaurar.Executar;
begin
  with TACBrObjetoACBr(fpObjetoDono) do
  begin
    FrmACBrMonitor.Restaurar1Click( FrmACBrMonitor );

  end;
end;

{ TMetodoRun }

{ Params: 0 - String com a linha de comando a ser executada
  Params: 1 - String com os parametros complementares a linha de comando (opcional)
  Params: 2 - Boolean - aguarda ou não a execussão do comando para continuar com o Monitor (opcional)
  params: 3 - Numerico - define o estado da janela  Utilize: 0 = Escondido; 1 = Normal (default); 2 = Minimizado; 3 = Maximizado (Disponível apenas no        Windows)
  params: 4 - Boolean -  Quando informado “True”, envia ALT+TAB para o teclado do Windows assim que a execução do programa terminar. Útil para restaurar o foco da aplicação controladora do ACBrMonitor (Disponível apenas no Windows)
}
procedure TMetodoRun.Executar;
var
  AComando: String;
  AParametros: String;
  AAguarda: Boolean;
  AWindowsState: Integer;
  AAltTab: Boolean;
begin
  AComando := fpCmd.Params(0);
  AParametros := fpCmd.Params(1);
  AAguarda := StrToBoolDef( fpCmd.Params(2), False );
  AWindowsState := StrToIntDef( fpCmd.Params(3), 1);
  AAltTab := StrToBoolDef(fpCmd.Params(4),False) ;


  with TACBrObjetoACBr(fpObjetoDono) do
  begin
    VerificaPermiteComandosRemoto ;

    if AComando = '' then
       raise Exception.Create('Linha de comando não informada');

    RunCommand( AComando,                          { Linha de comando }
                AParametros,                       { Parametros adicionais }
                AAguarda,                          { Aguarda termino execuçao ? }
                AWindowsState );                   { Estado da Janela }
    {$IFDEF MSWINDOWS}
    if AAltTab then
       SendKeys(pchar('%{TAB}'), False);
    {$ENDIF}

  end;

end;

{ TMetodoAtivo }

procedure TMetodoAtivo.Executar;
begin
  with TACBrObjetoACBr(fpObjetoDono) do
  begin
    fpCmd.Resposta := 'ATIVO';

  end;
end;

{ TACBrObjetoACBr }

constructor TACBrObjetoACBr.Create(AConfig: TMonitorConfig);
begin
  inherited Create(AConfig);

  ListaDeMetodos.Add(CMetodoAtivo);
  ListaDeMetodos.Add(CMetodoRun);
  ListaDeMetodos.Add(CMetodoRestaurar);
  ListaDeMetodos.Add(CMetodoOcultar);
  ListaDeMetodos.Add(CMetodoEncerrarmonitor);
  ListaDeMetodos.Add(CMetodoSendKeys);
  ListaDeMetodos.Add(CMetodoAppActivate);
  ListaDeMetodos.Add(CMetodoAppExists);
  ListaDeMetodos.Add(CMetodoBlockInput);
  ListaDeMetodos.Add(CMetodoSavetofile);
  ListaDeMetodos.Add(CMetodoLoadfromfile);
  ListaDeMetodos.Add(CMetodoFileExists);
  ListaDeMetodos.Add(CMetodoCopyFile);
  ListaDeMetodos.Add(CMetodoDeleteFiles);
  ListaDeMetodos.Add(CMetodoSetcertificado);
  ListaDeMetodos.Add(CMetodoSetWebservice);
  ListaDeMetodos.Add(CMetodoObterCertificados);
  ListaDeMetodos.Add(CMetodoLerini);
  ListaDeMetodos.Add(CMetodoAjustaLnhasLog);
  ListaDeMetodos.Add(CMetodoEncodeBase64);
  ListaDeMetodos.Add(CMetodoDecodeBase64);
  ListaDeMetodos.Add(CMetodoRoundABNT);
  ListaDeMetodos.Add(CMetodoVersao);
  ListaDeMetodos.Add(CMetodoDatahora);
  ListaDeMetodos.Add(CMetodoData);
  ListaDeMetodos.Add(CMetodoHora);
  ListaDeMetodos.Add(CMetodoExit);
  ListaDeMetodos.Add(CMetodoBye);
  ListaDeMetodos.Add(CMetodoSair);
  ListaDeMetodos.Add(CMetodoFim);
  ListaDeMetodos.Add(CMetodoSetTimeZone);

end;

procedure TACBrObjetoACBr.Executar(ACmd: TACBrCmd);
var
   AMetodoClass: TACBrMetodoClass;
   CmdNum: Integer;
   Ametodo: TACBrMetodo;
begin
  inherited Executar(ACmd);

  CmdNum := ListaDeMetodos.IndexOf(LowerCase(ACmd.Metodo));
  AMetodoClass := Nil;

  case CmdNum of
    0  : AMetodoClass := TMetodoAtivo;
    1  : AMetodoClass := TMetodoRun;
    2  : AMetodoClass := TMetodoRestaurar;
    3  : AMetodoClass := TMetodoOcultar;
    4  : AMetodoClass := TMetodoEncerrarmonitor;
    5  : AMetodoClass := TMetodoSendKeys;
    6  : AMetodoClass := TMetodoAppActivate;
    7  : AMetodoClass := TMetodoAppExists;
    8  : AMetodoClass := TMetodoBlockInput;
    9  : AMetodoClass := TMetodoSavetofile;
    10  : AMetodoClass := TMetodoLoadfromfile;
    11  : AMetodoClass := TMetodoFileExists;
    12  : AMetodoClass := TMetodoCopyFile;
    13  : AMetodoClass := TMetodoDeleteFiles;
    14  : AMetodoClass := TMetodoSetcertificado;
    15  : AMetodoClass := TMetodoSetWebservice;
    16  : AMetodoClass := TMetodoObterCertificados;
    17  : AMetodoClass := TMetodoLerini;
    18  : AMetodoClass := TMetodoAjustaLnhasLog;
    19  : AMetodoClass := TMetodoEncodeBase64;
    20  : AMetodoClass := TMetodoDecodeBase64;
    21  : AMetodoClass := TMetodoRoundABNT;
    22  : AMetodoClass := TMetodoVersao;
    23  : AMetodoClass := TMetodoDatahora;
    24  : AMetodoClass := TMetodoData;
    25  : AMetodoClass := TMetodoHora;
    26  : AMetodoClass := TMetodoExit;
    27  : AMetodoClass := TMetodoBye;
    28  : AMetodoClass := TMetodoSair;
    29  : AMetodoClass := TMetodoFim;
    30  : AMetodoClass := TMetodoSetTimeZone;

    else
      raise Exception.Create('Comando inválido ('+ copy(ACmd.Comando,6,length(ACmd.Comando))+')') ;

  end;

  if Assigned(AMetodoClass) then
  begin
    Ametodo := AMetodoClass.Create(ACmd, Self);
    try
      Ametodo.Executar;
    finally
      Ametodo.Free;
    end;

  end;
end;

procedure TACBrObjetoACBr.EncerrarMonitor;
begin
  fpCmd.Resposta := 'Obrigado por usar o ACBrMonitor' ;
  {$IFNDEF NOGUI}
    FrmACBrMonitor.mCmd.Lines.Clear;
  {$ELSE}
    WriteLn( 'Obrigado por usar o ACBrMonitorConsole' ) ;
  {$ENDIF}

  if Assigned( FrmACBrMonitor.Conexao ) then
    FrmACBrMonitor.Conexao.CloseSocket ;

end;

procedure TACBrObjetoACBr.VerificaPermiteComandosRemoto;
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

end;


end.

