{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                }
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

unit ACBrLibNFeDataModule;

{$mode delphi}

interface

uses
  Classes, SysUtils,
  ACBrNFe, ACBrNFeDANFeRLClass, ACBrMail, ACBrPosPrinter,
  ACBrDANFCeFortesFrA4, ACBrNFeDANFeESCPOS, ACBrDANFCeFortesFr,
  ACBrLibDataModule, ACBrLibConfig, ACBrLibComum;

type

  { TLibNFeDM }

  TLibNFeDM = class(TLibDataModule)
    ACBrMail1: TACBrMail;
    ACBrNFe1: TACBrNFe;
    ACBrPosPrinter1: TACBrPosPrinter;
  private
    DANFCeFortes: TACBrNFeDANFCeFortes;
    DANFCeA4: TACBrNFeDANFCeFortesA4;
    DANFCeEscPos: TACBrNFeDANFeESCPOS;
    NFeDANFe: TACBrNFeDANFeRL;

  public
    procedure AplicarConfiguracoes; override;
    procedure AplicarConfigMail;
    procedure AplicarConfigPosPrinter;
    procedure ConfigurarImpressao(NomeImpressora: String = ''; GerarPDF: Boolean = False;
                                  Protocolo: String = ''; MostrarPreview: String = ''; MarcaDagua: String = '';
                                  ViaConsumidor: String = ''; Simplificado: String = '');
    procedure FinalizarImpressao;
  end;

implementation

uses
  pcnConversao, pcnConversaoNFe,
  FileUtil, ACBrDeviceSerial, ACBrNFeDANFEClass,
  {$IFDEF Demo}ACBrNFeNotasFiscais, ACBrNFe.EnvEvento,{$ENDIF}
  ACBrDeviceConfig, ACBrLibNFeConfig, ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings;

{$R *.lfm}

{ TLibNFeDM }

procedure TLibNFeDM.AplicarConfiguracoes;
var
  pLibConfig: TLibNFeConfig;
begin
  ACBrNFe1.SSL.DescarregarCertificado;
  pLibConfig := TLibNFeConfig(Lib.Config);
  ACBrNFe1.Configuracoes.Assign(pLibConfig.NFe);

{$IFDEF Demo}
  GravarLog('Modo DEMO - Forçando ambiente para Homologação', logNormal);
  ACBrNFe1.Configuracoes.WebServices.Ambiente := taHomologacao;
{$ENDIF}

  AplicarConfigMail;
  AplicarConfigPosPrinter;
end;

procedure TLibNFeDM.AplicarConfigMail;
begin
  with ACBrMail1 do
  begin
    Attempts := Lib.Config.Email.Tentativas;
    SetTLS := Lib.Config.Email.TLS;
    DefaultCharset := Lib.Config.Email.Codificacao;
    From := Lib.Config.Email.Conta;
    FromName := Lib.Config.Email.Nome;
    SetSSL := Lib.Config.Email.SSL;
    Host := Lib.Config.Email.Servidor;
    IDECharset := Lib.Config.Email.Codificacao;
    IsHTML := Lib.Config.Email.IsHTML;
    Password := Lib.Config.Email.Senha;
    Port := IntToStr(Lib.Config.Email.Porta);
    Priority := Lib.Config.Email.Priority;
    ReadingConfirmation := Lib.Config.Email.Confirmacao;
    DeliveryConfirmation := Lib.Config.Email.ConfirmacaoEntrega;
    TimeOut := Lib.Config.Email.TimeOut;
    Username := Lib.Config.Email.Usuario;
    UseThread := Lib.Config.Email.SegundoPlano;
  end;
end;

procedure TLibNFeDM.AplicarConfigPosPrinter;
var
  LibConfig: TLibNFeConfig;
begin
  LibConfig := TLibNFeConfig(Lib.Config);

  with ACBrPosPrinter1 do
  begin
    ArqLog := LibConfig.PosPrinter.ArqLog;
    Modelo := TACBrPosPrinterModelo(LibConfig.PosPrinter.Modelo);
    Porta := LibConfig.PosPrinter.Porta;
    PaginaDeCodigo := TACBrPosPaginaCodigo(LibConfig.PosPrinter.PaginaDeCodigo);
    ColunasFonteNormal := LibConfig.PosPrinter.ColunasFonteNormal;
    EspacoEntreLinhas := LibConfig.PosPrinter.EspacoEntreLinhas;
    LinhasEntreCupons := LibConfig.PosPrinter.LinhasEntreCupons;
    CortaPapel := LibConfig.PosPrinter.CortaPapel;
    TraduzirTags := LibConfig.PosPrinter.TraduzirTags;
    IgnorarTags := LibConfig.PosPrinter.IgnorarTags;
    LinhasBuffer := LibConfig.PosPrinter.LinhasBuffer;
    ControlePorta := LibConfig.PosPrinter.ControlePorta;
    VerificarImpressora := LibConfig.PosPrinter.VerificarImpressora;

    ConfigBarras.MostrarCodigo := LibConfig.PosPrinter.BcMostrarCodigo;
    ConfigBarras.LarguraLinha := LibConfig.PosPrinter.BcLarguraLinha;
    ConfigBarras.Altura := LibConfig.PosPrinter.BcAltura;
    ConfigBarras.Margem := LibConfig.PosPrinter.BcMargem;

    ConfigQRCode.Tipo := LibConfig.PosPrinter.QrTipo;
    ConfigQRCode.LarguraModulo := LibConfig.PosPrinter.QrLarguraModulo;
    ConfigQRCode.ErrorLevel := LibConfig.PosPrinter.QrErrorLevel;

    ConfigLogo.IgnorarLogo := LibConfig.PosPrinter.LgIgnorarLogo;
    ConfigLogo.KeyCode1 := LibConfig.PosPrinter.LgKeyCode1;
    ConfigLogo.KeyCode2 := LibConfig.PosPrinter.LgKeyCode2;
    ConfigLogo.FatorX := LibConfig.PosPrinter.LgFatorX;
    ConfigLogo.FatorY := LibConfig.PosPrinter.LgFatorY;

    ConfigGaveta.SinalInvertido := LibConfig.PosPrinter.GvSinalInvertido;
    ConfigGaveta.TempoON := LibConfig.PosPrinter.GvTempoON;
    ConfigGaveta.TempoOFF := LibConfig.PosPrinter.GvTempoOFF;

    ConfigModoPagina.Largura := LibConfig.PosPrinter.MpLargura;
    ConfigModoPagina.Altura := LibConfig.PosPrinter.MpAltura;
    ConfigModoPagina.Esquerda := LibConfig.PosPrinter.MpEsquerda;
    ConfigModoPagina.Topo := LibConfig.PosPrinter.MpTopo;
    ConfigModoPagina.Direcao := TACBrPosDirecao(LibConfig.PosPrinter.MpDirecao);
    ConfigModoPagina.EspacoEntreLinhas := LibConfig.PosPrinter.MpEspacoEntreLinhas;

    Device.Baud := LibConfig.PosDeviceConfig.Baud;
    Device.Data := LibConfig.PosDeviceConfig.Data;
    Device.TimeOut := LibConfig.PosDeviceConfig.TimeOut;
    Device.Parity := TACBrSerialParity(LibConfig.PosDeviceConfig.Parity);
    Device.Stop := TACBrSerialStop(LibConfig.PosDeviceConfig.Stop);
    Device.MaxBandwidth := LibConfig.PosDeviceConfig.MaxBandwidth;
    Device.SendBytesCount := LibConfig.PosDeviceConfig.SendBytesCount;
    Device.SendBytesInterval := LibConfig.PosDeviceConfig.SendBytesInterval;
    Device.HandShake := TACBrHandShake(LibConfig.PosDeviceConfig.HandShake);
    Device.HardFlow := LibConfig.PosDeviceConfig.HardFlow;
    Device.SoftFlow := LibConfig.PosDeviceConfig.SoftFlow;
  end;
end;

procedure TLibNFeDM.ConfigurarImpressao(NomeImpressora: String; GerarPDF: Boolean = False;
  Protocolo: String = ''; MostrarPreview: String = ''; MarcaDagua: String = '';
  ViaConsumidor: String = ''; Simplificado: String = '');
var
  LibConfig: TLibNFeConfig;
{$IFDEF Demo}
  I: Integer;
  ANota: NotaFiscal;
  AEvento: TInfEventoCollectionItem;
{$ENDIF}
begin
  LibConfig := TLibNFeConfig(Lib.Config);

  GravarLog('ConfigurarImpressao - Iniciado', logNormal);

  if ACBrNFe1.NotasFiscais.Count > 0 then
  begin
    if ACBrNFe1.NotasFiscais.Items[0].NFe.Ide.modelo = 65 then
    begin
      if GerarPDF and not (LibConfig.DANFe.NFCe.TipoRelatorioBobina in [tpFortes, tpFortesA4]) then
      begin
        DANFCeFortes := TACBrNFeDANFCeFortes.Create(nil);
        ACBrNFe1.DANFE := DANFCeFortes
      end
      else if LibConfig.DANFe.NFCe.TipoRelatorioBobina = tpFortes then
      begin
        DANFCeFortes := TACBrNFeDANFCeFortes.Create(nil);
        ACBrNFe1.DANFE := DANFCeFortes
      end
      else if LibConfig.DANFe.NFCe.TipoRelatorioBobina = tpFortesA4 then
      begin
        DANFCeA4 := TACBrNFeDANFCeFortesA4.Create(nil);
        ACBrNFe1.DANFE := DANFCeA4
      end
      else
      begin
         DANFCeEscPos := TACBrNFeDANFeESCPOS.Create(nil);
         DANFCeEscPos.PosPrinter := ACBrPosPrinter1;
         ACBrNFe1.DANFE := DANFCeEscPos;
      end;
    end
    else
    begin
       NFeDANFe := TACBrNFeDANFeRL.Create(nil);
       ACBrNFe1.DANFE := NFeDANFe;
       NFeDANFe.ImprimeDescAcrescItem:=LibConfig.DANFe.NFe.ImprimeDescAcrescItemNFe;
    end;

    if (ACBrNFe1.NotasFiscais.Items[0].NFe.procNFe.cStat in [101, 151, 155]) then
      ACBrNFe1.DANFE.Cancelada := True
    else
      ACBrNFe1.DANFE.Cancelada := False;
  end
  else
  begin
    NFeDANFe := TACBrNFeDANFeRL.Create(nil);
    ACBrNFe1.DANFE := NFeDANFe;
  end;

  if GerarPDF then
  begin
    if (LibConfig.DANFe.PathPDF <> '') then
      if not DirectoryExists(PathWithDelim(LibConfig.DANFe.PathPDF))then
        ForceDirectories(PathWithDelim(LibConfig.DANFe.PathPDF));
  end;

  LibConfig.DANFe.Apply(ACBrNFe1.DANFE, Lib);

{$IFDEF Demo}
  for I:= 0 to ACBrNFe1.NotasFiscais.Count -1 do
  begin
    ANota := ACBrNFe1.NotasFiscais.Items[I];
    ANota.NFe.Ide.tpAmb := taHomologacao;
  end;

  for I:= 0 to ACBrNFe1.EventoNFe.Evento.Count -1 do
  begin
    AEvento := ACBrNFe1.EventoNFe.Evento.Items[I];
    AEvento.InfEvento.tpAmb := taHomologacao;
  end;
{$ENDIF}

  if NaoEstaVazio(NomeImpressora) then
    ACBrNFe1.DANFE.Impressora := NomeImpressora;

  if NaoEstaVazio(MostrarPreview) then
    ACBrNFe1.DANFE.MostraPreview := StrToBoolDef(MostrarPreview, False);

  if NaoEstaVazio(Protocolo) then
    ACBrNFe1.DANFE.Protocolo := Protocolo
  else
    ACBrNFe1.DANFE.Protocolo := '';

  if ACBrNFe1.DANFE is TACBrNFeDANFCEClass then
  begin
     if NaoEstaVazio(ViaConsumidor) then
       TACBrNFeDANFCEClass(ACBrNFe1.DANFE).ViaConsumidor := StrToBoolDef(ViaConsumidor, False);

     if ACBrNFe1.DANFE = DANFCeEscPos then
     begin
        if not DANFCeEscPos.PosPrinter.ControlePorta then
        begin
          DANFCeEscPos.PosPrinter.Ativar;
          if not DANFCeEscPos.PosPrinter.Device.Ativo then
            DANFCeEscPos.PosPrinter.Device.Ativar;
        end;
     end;
  end
  else
  begin
    if NaoEstaVazio(Simplificado) then
    begin
      if StrToBoolDef(Simplificado, False) then
        NFeDANFe.TipoDANFE := tiSimplificado;
    end;

     if NaoEstaVazio(MarcaDagua) then
       NFeDANFe.MarcaDagua := MarcaDagua
     else
       NFeDANFe.MarcaDagua := '';
  end;

  GravarLog('ConfigurarImpressao - Feito', logNormal);
end;

procedure TLibNFeDM.FinalizarImpressao;
begin
  GravarLog('FinalizarImpressao - Iniciado', logNormal);

  if ACBrPosPrinter1.Ativo then
          ACBrPosPrinter1.Desativar;

  ACBrNFe1.DANFE := nil;
  if Assigned(NFeDANFe) then FreeAndNil(NFeDANFe);
  if Assigned(DANFCeFortes) then FreeAndNil(DANFCeFortes);
  if Assigned(DANFCeA4) then FreeAndNil(DANFCeA4);
  if Assigned(DANFCeEscPos) then FreeAndNil(DANFCeEscPos);

  GravarLog('FinalizarImpressao - Feito', logNormal);
end;

end.
