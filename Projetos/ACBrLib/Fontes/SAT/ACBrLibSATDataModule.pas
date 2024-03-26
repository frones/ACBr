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

{$I ACBr.inc}

unit ACBrLibSatDataModule;

interface

uses
  Classes, SysUtils, ACBrLibConfig, ACBrSAT,
  ACBrSATExtratoClass, ACBrSATExtratoESCPOS, ACBrSATExtratoFortesFr,
  ACBrMail, ACBrPosPrinter, ACBrLibComum, ACBrLibDataModule, ACBrSATClass;

type

  { TLibSatDM }

  TLibSatDM = class(TLibDataModule)
    ACBrMail1: TACBrMail;
    ACBrPosPrinter1: TACBrPosPrinter;
    ACBrSAT1: TACBrSAT;

    procedure ACBrSAT1GetcodigoDeAtivacao(var Chave: AnsiString);
    procedure ACBrSAT1GetNumeroSessao(var NumeroSessao: Integer);
    procedure ACBrSAT1GetsignAC(var Chave: AnsiString);
  private
    ExtratoEscPos: TACBrSATExtratoESCPOS;
    ExtratoFortes: TACBrSATExtratoFortes;

  public
    procedure AplicarConfiguracoes; override;
    procedure AplicarConfigMail;
    procedure AplicarConfigPosPrinter;
    procedure ConfigurarImpressao(NomeImpressora: String = ''; GerarPDF: Boolean = False; NomeArqPDF: String = '');
    procedure FinalizarImpressao;
    function GerarImpressaoFiscalMFe: Ansistring;
    procedure CarregarDadosVenda(XmlArquivoOuString: Ansistring);
    procedure CarregarDadosCancelamento(XmlArquivoOuString: Ansistring);

  end;

implementation

uses
  FileUtil, pcnConversao,
  ACBrDeviceConfig, ACBrDeviceSerial, ACBrDFeSSL,
  ACBrUtil.FilesIO, ACBrUtil.Strings,
  ACBrLibSATConfig;

{$R *.lfm}

{ TLibSatDM }

procedure TLibSatDM.ACBrSAT1GetcodigoDeAtivacao(var Chave: AnsiString);
begin
  Chave := TLibSATConfig(Lib.Config).CodigoDeAtivacao;
end;

procedure TLibSatDM.ACBrSAT1GetsignAC(var Chave: AnsiString);
begin
  Chave := TLibSATConfig(Lib.Config).SignAC;
end;

procedure TLibSatDM.ACBrSAT1GetNumeroSessao(var NumeroSessao: Integer);
begin
  if ACBrSAT1.Tag <> 0 then
  NumeroSessao:= ACBrSAT1.Tag;

  ACBrSAT1.Tag:= 0;
end;

procedure TLibSatDM.AplicarConfiguracoes;
var
  LibConfig: TLibSATConfig;
begin
  LibConfig := TLibSATConfig(Lib.Config);

  with ACBrSAT1 do
  begin
    Modelo := LibConfig.Modelo;
    NomeDLL := LibConfig.NomeDLL;
    ArqLOG := LibConfig.ArqLOG;
    ValidarNumeroSessaoResposta := LibConfig.ValidarNumeroSessaoResposta;
    NumeroTentativasValidarSessao := LibConfig.NumeroTentativasValidarSessao;

    with Config do
    begin
      infCFe_versaoDadosEnt := LibConfig.Config.infCFe_versaoDadosEnt;
      ide_CNPJ := LibConfig.Config.ide_CNPJ;
      ide_numeroCaixa := LibConfig.Config.ide_numeroCaixa;
      ide_tpAmb := LibConfig.Config.ide_tpAmb;
      emit_CNPJ := LibConfig.Config.emit_CNPJ;
      emit_IE := LibConfig.Config.emit_IE;
      emit_IM := LibConfig.Config.emit_IM;
      emit_cRegTrib := LibConfig.Config.emit_cRegTrib;
      emit_cRegTribISSQN := LibConfig.Config.emit_cRegTribISSQN;
      emit_indRatISSQN := LibConfig.Config.emit_indRatISSQN;
      EhUTF8 := LibConfig.Config.EhUTF8;
      PaginaDeCodigo := LibConfig.Config.PaginaDeCodigo;
      ArqSchema:= LibConfig.Config.ArqSchema;
      XmlSignLib:= LibConfig.Config.XmlSignLib;
    end;

    with SSL do
    begin
      SSLCryptLib := LibConfig.Certificado.SSLCryptLib;
      ArquivoPFX := LibConfig.Certificado.ArquivoPFX;
      NumeroSerie := LibConfig.Certificado.NumeroSerie;
      Senha := LibConfig.Certificado.Senha;
    end;

    with ConfigArquivos do
    begin
      SalvarCFe := LibConfig.Arquivos.SalvarCFe;
      SalvarCFeCanc := LibConfig.Arquivos.SalvarCFeCanc;
      SalvarEnvio := LibConfig.Arquivos.SalvarEnvio;
      SepararPorCNPJ := LibConfig.Arquivos.SepararPorCNPJ;
      SepararPorModelo := LibConfig.Arquivos.SepararPorModelo;
      SepararPorAno := LibConfig.Arquivos.SepararPorAno;
      SepararPorMes := LibConfig.Arquivos.SepararPorMes;
      SepararPorDia := LibConfig.Arquivos.SepararPorDia;
      PastaCFeVenda := LibConfig.Arquivos.PastaCFeVenda;
      PastaCFeCancelamento := LibConfig.Arquivos.PastaCFeCancelamento;
      PastaEnvio := LibConfig.Arquivos.PastaEnvio;
      PrefixoArqCFe := LibConfig.Arquivos.PrefixoArqCFe;
      PrefixoArqCFeCanc := LibConfig.Arquivos.PrefixoArqCFeCanc;
    end;

    with Rede do
    begin
      tipoInter := LibConfig.Rede.tipoInter;
      SSID := LibConfig.Rede.SSID;
      seg := LibConfig.Rede.seg;
      codigo := LibConfig.Rede.codigo;
      tipoLan := LibConfig.Rede.tipoLan;
      lanIP := LibConfig.Rede.lanIP;
      lanMask := LibConfig.Rede.lanMask;
      lanGW := LibConfig.Rede.lanGW;
      lanDNS1 := LibConfig.Rede.lanDNS1;
      lanDNS2 := LibConfig.Rede.lanDNS2;
      usuario := LibConfig.Rede.usuario;
      senha := B64CryptToString(LibConfig.Rede.senha, LibConfig.ChaveCrypt);
      proxy := LibConfig.Rede.proxy;
      proxy_ip := LibConfig.ProxyInfo.Servidor;
      proxy_porta := LibConfig.ProxyInfo.Porta;
      proxy_user := LibConfig.ProxyInfo.Usuario;
      proxy_senha := LibConfig.ProxyInfo.Senha;
    end;

    SSL.SSLXmlSignLib := xsLibXml2;
    SSL.SSLCryptLib := cryOpenSSL;

    AplicarConfigMail;
    AplicarConfigPosPrinter;
  end;
end;

procedure TLibSatDM.AplicarConfigMail;
begin;
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

procedure TLibSatDM.AplicarConfigPosPrinter;
var
  LibConfig: TLibSATConfig;
begin
  LibConfig := TLibSATConfig(Lib.Config);

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

procedure TLibSatDM.ConfigurarImpressao(NomeImpressora: String; GerarPDF: Boolean; NomeArqPDF: String);
var
  LibConfig: TLibSATConfig;
begin
  LibConfig := TLibSATConfig(Lib.Config);

  with LibConfig.Extrato do
  begin
    if GerarPDF or (TipoExtrato = teFortes) then
    begin
      ExtratoFortes := TACBrSATExtratoFortes.Create(nil);
      ACBrSAT1.Extrato := ExtratoFortes;
    end
    else
    begin
      ExtratoEscPos := TACBrSATExtratoESCPOS.Create(nil);
      ExtratoEscPos.PosPrinter := ACBrPosPrinter1;
      ACBrSAT1.Extrato := ExtratoEscPos;
    end;

    LibConfig.Extrato.Apply(ACBrSAT1.Extrato, Lib);

{$IFDEF Demo}
    ACBrSAT1.CFe.ide.tpAmb := taHomologacao;
{$ENDIF}

    if(NomeImpressora <> '') then
      ACBrSAT1.Extrato.Impressora := NomeImpressora;

    if GerarPDF then
    begin
      ACBrSAT1.Extrato.Filtro := fiPDF;

      if (NomeArqPDF <> '') then
        ACBrSAT1.Extrato.NomeDocumento := NomeArqPDF;

      if (LibConfig.Extrato.PathPDF <> '') then
        if not DirectoryExists(PathWithDelim(LibConfig.Extrato.PathPDF))then
          ForceDirectories(PathWithDelim(LibConfig.Extrato.PathPDF));
    end;
  end;
end;

procedure TLibSatDM.FinalizarImpressao;
begin
  GravarLog('FinalizarImpressao - Iniciado', logNormal);

  if ACBrPosPrinter1.Ativo then
    ACBrPosPrinter1.Desativar;

  ACBrSAT1.Extrato := nil;
  if Assigned(ExtratoFortes) then FreeAndNil(ExtratoFortes);
  if Assigned(ExtratoEscPos) then FreeAndNil(ExtratoEscPos);

  GravarLog('FinalizarImpressao - Feito', logNormal);
end;

function TLibSatDM.GerarImpressaoFiscalMFe: Ansistring;
var
  LibConfig: TLibSATConfig;
begin
  LibConfig := TLibSATConfig(Lib.Config);

  try
    ExtratoEscPos := TACBrSATExtratoESCPOS.Create(nil);
    ExtratoEscPos.PosPrinter := ACBrPosPrinter1;
    ACBrSAT1.Extrato := ExtratoEscPos;
    LibConfig.Extrato.Apply(ExtratoEscPos, Lib);
    Result := ExtratoEscPos.GerarImpressaoFiscalMFe;
  finally
    FinalizarImpressao;
  end;
end;

procedure TLibSatDM.CarregarDadosVenda(XmlArquivoOuString: Ansistring);
begin
  if Trim(XmlArquivoOuString) = '' then exit;

  if FileExists(XmlArquivoOuString) then
  begin
    GravarLog('Carregando arquivo xml [' + XmlArquivoOuString + ']', logParanoico);
    ACBrSAT1.CFe.LoadFromFile(XmlArquivoOuString);
  end
  else
  begin
    GravarLog('Carregando xml string  [' + XmlArquivoOuString + ']', logParanoico);
    ACBrSAT1.CFe.AsXMLString := XmlArquivoOuString;
  end;

{$IFDEF Demo}
  ACBrSAT1.CFe.ide.tpAmb := taHomologacao;
{$ENDIF}
end;

procedure TLibSatDM.CarregarDadosCancelamento(XmlArquivoOuString: Ansistring);
begin
  if Trim(XmlArquivoOuString) = '' then exit;

  if FileExists(XmlArquivoOuString) then
  begin
    GravarLog('Carregando arquivo xml cancelamento [' + XmlArquivoOuString + ']', logParanoico);
    ACBrSAT1.CFeCanc.LoadFromFile(XmlArquivoOuString);
  end
  else
  begin
    GravarLog('Carregando xml string de cancelamento  [' + XmlArquivoOuString + ']', logParanoico);
    ACBrSAT1.CFeCanc.AsXMLString := XmlArquivoOuString;
  end;
end;


end.

