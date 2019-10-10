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

unit ACBrLibSatDataModule;

{$mode delphi}

interface

uses
  Classes, SysUtils, syncobjs, ACBrLibConfig, ACBrSAT, ACBrIntegrador,
  ACBrSATExtratoClass, ACBrSATExtratoESCPOS, ACBrSATExtratoFortesFr,
  ACBrMail, ACBrPosPrinter, ACBrSATClass,
  ACBrLibMailImport, ACBrLibPosPrinterImport;

type

  { TLibSatDM }

  TLibSatDM = class(TDataModule)
    ACBrIntegrador1: TACBrIntegrador;
    ACBrSAT1: TACBrSAT;
    ACBrSATExtratoESCPOS1: TACBrSATExtratoESCPOS;
    ACBrSATExtratoFortes1: TACBrSATExtratoFortes;

    procedure ACBrSAT1GetcodigoDeAtivacao(var Chave: AnsiString);
    procedure ACBrSAT1GetsignAC(var Chave: AnsiString);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FLock: TCriticalSection;
    FACBrMail: TACBrMail;
    FACBrPosPrinter: TACBrPosPrinter;

    FLibMail: TACBrLibMail;
    FLibPosPrinter: TACBrLibPosPrinter;
  public
    procedure CriarACBrMail;
    procedure CriarACBrPosPrinter;

    procedure AplicarConfiguracoes;
    procedure AplicarConfigMail;
    procedure AplicarConfigPosPrinter;
    procedure ConfigurarImpressao(NomeImpressora: String = ''; GerarPDF: Boolean = False);
    procedure CarregarDadosVenda(XmlArquivoOuString: String; aNomePDF: String = '');
    procedure CarregarDadosCancelamento(aStr: String; aNomePDF: String = '');
    procedure GravarLog(AMsg: String; NivelLog: TNivelLog; Traduzir: Boolean = False);
    procedure Travar;
    procedure Destravar;

    function RespostaIntegrador: String;

  end;

implementation

uses
  strutils, FileUtil, ACBrDeviceConfig, ACBrLibConsts,
  ACBrUtil, ACBrLibSATConfig, ACBrLibComum, ACBrLibIntegradorResposta;

{$R *.lfm}

{ TLibSatDM }

procedure TLibSatDM.DataModuleCreate(Sender: TObject);
begin
  FLock := TCriticalSection.Create;
  FACBrMail := Nil;
  FLibMail := Nil;
  FACBrPosPrinter := Nil;
  FLibPosPrinter := Nil;
end;

procedure TLibSatDM.DataModuleDestroy(Sender: TObject);
begin
  FLock.Destroy;

  if Assigned(FLibMail) then
    FreeAndNil(FLibMail)
  else if Assigned(FACBrMail) then
    FreeAndNil(FACBrMail);

  if Assigned(FLibPosPrinter) then
    FreeAndNil(FLibPosPrinter)
  else if Assigned(FACBrPosPrinter) then
    FreeAndNil(FACBrPosPrinter);
end;

procedure TLibSatDM.CriarACBrMail;
var
  NomeLib: String;
begin
  if Assigned(FLibMail) or Assigned(FACBrMail) then
    Exit;

  GravarLog('  CriarACBrMail', logCompleto);

  NomeLib := ApplicationPath + CACBrMailLIBName;
  if FileExists(NomeLib) then
  begin
    GravarLog('      Carregando MAIL de: '+NomeLib, logCompleto);
    // Criando Classe para Leitura da Lib //
    FLibMail  := TACBrLibMail.Create(NomeLib, pLib.Config.NomeArquivo, pLib.Config.ChaveCrypt);
    FACBrMail := FLibMail.ACBrMail;
  end
  else
  begin
    GravarLog('     Criando MAIL Interno', logCompleto);
    FACBrMail := TACBrMail.Create(Nil);
  end;

  ACBrSAT1.MAIL := FACBrMail;
end;

procedure TLibSatDM.CriarACBrPosPrinter;
var
  NomeLib: String;
begin
  if Assigned(FLibPosPrinter) or Assigned(FACBrPosPrinter) then
    Exit;

  GravarLog('  CriarACBrPosPrinter', logCompleto);

  NomeLib := ApplicationPath + CACBrPosPrinterLIBName;
  if FileExists(NomeLib) then
  begin
    GravarLog('      Carregando PosPrinter de: '+NomeLib, logCompleto);
    // Criando Classe para Leitura da Lib //
    FLibPosPrinter  := TACBrLibPosPrinter.Create(NomeLib, pLib.Config.NomeArquivo, pLib.Config.ChaveCrypt);
    FACBrPosPrinter := FLibPosPrinter.ACBrPosPrinter;
  end
  else
  begin
    GravarLog('     Criando PosPrinter Interno', logCompleto);
    FACBrPosPrinter := TACBrPosPrinter.Create(Nil);
    TLibSATConfig(pLib.Config).PosDeviceConfig := TDeviceConfig.Create(CSessaoPosPrinterDevice);
  end;

  ACBrSATExtratoESCPOS1.PosPrinter := FACBrPosPrinter;
end;

procedure TLibSatDM.ACBrSAT1GetcodigoDeAtivacao(var Chave: AnsiString);
begin
  Chave := TLibSATConfig(pLib.Config).CodigoDeAtivacao;
end;

procedure TLibSatDM.ACBrSAT1GetsignAC(var Chave: AnsiString);
begin
  Chave := TLibSATConfig(pLib.Config).SignAC;
end;

procedure TLibSatDM.AplicarConfiguracoes;
var
  pLibConfig: TLibSATConfig;
begin
  pLibConfig := TLibSATConfig(pLib.Config);

  with ACBrSAT1 do
  begin
    Modelo := pLibConfig.Modelo;
    NomeDLL := pLibConfig.NomeDLL;
    ArqLOG := pLibConfig.ArqLOG;
    ValidarNumeroSessaoResposta := pLibConfig.ValidarNumeroSessaoResposta;
    NumeroTentativasValidarSessao := pLibConfig.NumeroTentativasValidarSessao;

    with Config do
    begin
      infCFe_versaoDadosEnt := pLibConfig.Config.infCFe_versaoDadosEnt;
      ide_CNPJ := pLibConfig.Config.ide_CNPJ;
      ide_numeroCaixa := pLibConfig.Config.ide_numeroCaixa;
      ide_tpAmb := pLibConfig.Config.ide_tpAmb;
      emit_CNPJ := pLibConfig.Config.emit_CNPJ;
      emit_IE := pLibConfig.Config.emit_IE;
      emit_IM := pLibConfig.Config.emit_IM;
      emit_cRegTrib := pLibConfig.Config.emit_cRegTrib;
      emit_cRegTribISSQN := pLibConfig.Config.emit_cRegTribISSQN;
      emit_indRatISSQN := pLibConfig.Config.emit_indRatISSQN;
      EhUTF8 := pLibConfig.Config.EhUTF8;
      PaginaDeCodigo := pLibConfig.Config.PaginaDeCodigo;
      ArqSchema:= pLibConfig.Config.ArqSchema;
      XmlSignLib := pLibConfig.Config.XmlSignLib;
    end;

    with SSL do
    begin
      SSLCryptLib := pLibConfig.Certificado.SSLCryptLib;
      ArquivoPFX := pLibConfig.Certificado.ArquivoPFX;
      NumeroSerie := pLibConfig.Certificado.NumeroSerie;
      Senha := pLibConfig.Certificado.Senha;
    end;

    with ConfigArquivos do
    begin
      SalvarCFe := pLibConfig.Arquivos.SalvarCFe;
      SalvarCFeCanc := pLibConfig.Arquivos.SalvarCFeCanc;
      SalvarEnvio := pLibConfig.Arquivos.SalvarEnvio;
      SepararPorCNPJ := pLibConfig.Arquivos.SepararPorCNPJ;
      SepararPorModelo := pLibConfig.Arquivos.SepararPorModelo;
      SepararPorAno := pLibConfig.Arquivos.SepararPorAno;
      SepararPorMes := pLibConfig.Arquivos.SepararPorMes;
      SepararPorDia := pLibConfig.Arquivos.SepararPorDia;
      PastaCFeVenda := pLibConfig.Arquivos.PastaCFeVenda;
      PastaCFeCancelamento := pLibConfig.Arquivos.PastaCFeCancelamento;
      PastaEnvio := pLibConfig.Arquivos.PastaEnvio;
      PrefixoArqCFe := pLibConfig.Arquivos.PrefixoArqCFe;
      PrefixoArqCFeCanc := pLibConfig.Arquivos.PrefixoArqCFeCanc;
    end;

    with Rede do
    begin
      tipoInter := pLibConfig.Rede.tipoInter;
      SSID := pLibConfig.Rede.SSID;
      seg := pLibConfig.Rede.seg;
      codigo := pLibConfig.Rede.codigo;
      tipoLan := pLibConfig.Rede.tipoLan;
      lanIP := pLibConfig.Rede.lanIP;
      lanMask := pLibConfig.Rede.lanMask;
      lanGW := pLibConfig.Rede.lanGW;
      lanDNS1 := pLibConfig.Rede.lanDNS1;
      lanDNS2 := pLibConfig.Rede.lanDNS2;
      usuario := pLibConfig.Rede.usuario;
      senha := B64CryptToString(pLibConfig.Rede.senha, pLibConfig.ChaveCrypt);
      proxy := pLibConfig.Rede.proxy;
      proxy_ip := pLibConfig.ProxyInfo.Servidor;
      proxy_porta := pLibConfig.ProxyInfo.Porta;
      proxy_user := pLibConfig.ProxyInfo.Usuario;
      proxy_senha := pLibConfig.ProxyInfo.Senha;
    end;

    if pLibConfig.IsMFe then
    begin
      Integrador := ACBrIntegrador1;
      with Integrador do
      begin
        ArqLOG := pLibConfig.Integrador.ArqLOG;
        PastaInput := pLibConfig.Integrador.PastaInput;
        PastaOutput := pLibConfig.Integrador.PastaOutput;
        Timeout := pLibConfig.Integrador.Timeout;
      end;
    end
    else
      Integrador := nil;

    AplicarConfigMail;
    AplicarConfigPosPrinter;
  end;
end;

procedure TLibSatDM.AplicarConfigMail;
begin

  if Assigned(FLibMail) then
  begin
    FLibMail.ConfigLer(pLib.Config.NomeArquivo);
    Exit;
  end;

  with FACBrMail do
  begin
    Attempts := pLib.Config.Email.Tentativas;
    SetTLS := pLib.Config.Email.TLS;
    DefaultCharset := pLib.Config.Email.Codificacao;
    From := pLib.Config.Email.Conta;
    FromName := pLib.Config.Email.Nome;
    SetSSL := pLib.Config.Email.SSL;
    Host := pLib.Config.Email.Servidor;
    IDECharset := pLib.Config.Email.Codificacao;
    IsHTML := pLib.Config.Email.IsHTML;
    Password := pLib.Config.Email.Senha;
    Port := IntToStr(pLib.Config.Email.Porta);
    Priority := pLib.Config.Email.Priority;
    ReadingConfirmation := pLib.Config.Email.Confirmacao;
    DeliveryConfirmation := pLib.Config.Email.ConfirmacaoEntrega;
    TimeOut := pLib.Config.Email.TimeOut;
    Username := pLib.Config.Email.Usuario;
    UseThread := pLib.Config.Email.SegundoPlano;
  end;
end;

procedure TLibSatDM.AplicarConfigPosPrinter;
var
  pLibConfig: TLibSATConfig;
begin
  if Assigned(FLibPosPrinter) then
  begin
    FLibPosPrinter.ConfigLer(pLib.Config.NomeArquivo);
    Exit;
  end;

  pLibConfig := TLibSATConfig(pLib.Config);

  with FACBrPosPrinter do
  begin
    ArqLog := pLibConfig.PosPrinter.ArqLog;
    Modelo := TACBrPosPrinterModelo(pLibConfig.PosPrinter.Modelo);
    Porta := pLibConfig.PosPrinter.Porta;
    PaginaDeCodigo := TACBrPosPaginaCodigo(pLibConfig.PosPrinter.PaginaDeCodigo);
    ColunasFonteNormal := pLibConfig.PosPrinter.ColunasFonteNormal;
    EspacoEntreLinhas := pLibConfig.PosPrinter.EspacoEntreLinhas;
    LinhasEntreCupons := pLibConfig.PosPrinter.LinhasEntreCupons;
    CortaPapel := pLibConfig.PosPrinter.CortaPapel;
    TraduzirTags := pLibConfig.PosPrinter.TraduzirTags;
    IgnorarTags := pLibConfig.PosPrinter.IgnorarTags;
    LinhasBuffer := pLibConfig.PosPrinter.LinhasBuffer;
    ControlePorta := pLibConfig.PosPrinter.ControlePorta;
    VerificarImpressora := pLibConfig.PosPrinter.VerificarImpressora;

    ConfigBarras.MostrarCodigo := pLibConfig.PosPrinter.BcMostrarCodigo;
    ConfigBarras.LarguraLinha := pLibConfig.PosPrinter.BcLarguraLinha;
    ConfigBarras.Altura := pLibConfig.PosPrinter.BcAltura;
    ConfigBarras.Margem := pLibConfig.PosPrinter.BcMargem;

    ConfigQRCode.Tipo := pLibConfig.PosPrinter.QrTipo;
    ConfigQRCode.LarguraModulo := pLibConfig.PosPrinter.QrLarguraModulo;
    ConfigQRCode.ErrorLevel := pLibConfig.PosPrinter.QrErrorLevel;

    ConfigLogo.IgnorarLogo := pLibConfig.PosPrinter.LgIgnorarLogo;
    ConfigLogo.KeyCode1 := pLibConfig.PosPrinter.LgKeyCode1;
    ConfigLogo.KeyCode2 := pLibConfig.PosPrinter.LgKeyCode2;
    ConfigLogo.FatorX := pLibConfig.PosPrinter.LgFatorX;
    ConfigLogo.FatorY := pLibConfig.PosPrinter.LgFatorY;

    ConfigGaveta.SinalInvertido := pLibConfig.PosPrinter.GvSinalInvertido;
    ConfigGaveta.TempoON := pLibConfig.PosPrinter.GvTempoON;
    ConfigGaveta.TempoOFF := pLibConfig.PosPrinter.GvTempoOFF;

    ConfigModoPagina.Largura := pLibConfig.PosPrinter.MpLargura;
    ConfigModoPagina.Altura := pLibConfig.PosPrinter.MpAltura;
    ConfigModoPagina.Esquerda := pLibConfig.PosPrinter.MpEsquerda;
    ConfigModoPagina.Topo := pLibConfig.PosPrinter.MpTopo;
    ConfigModoPagina.Direcao := TACBrPosDirecao(pLibConfig.PosPrinter.MpDirecao);
    ConfigModoPagina.EspacoEntreLinhas := pLibConfig.PosPrinter.MpEspacoEntreLinhas;

    pLibConfig.PosDeviceConfig.Assign(Device);
  end;
end;

procedure TLibSatDM.GravarLog(AMsg: String; NivelLog: TNivelLog;
  Traduzir: Boolean);
begin
  if Assigned(pLib) then
    pLib.GravarLog(AMsg, NivelLog, Traduzir);
end;

procedure TLibSatDM.ConfigurarImpressao(NomeImpressora: String; GerarPDF: Boolean);
var
  pLibConfig: TLibSATConfig;
begin
  pLibConfig := TLibSATConfig(pLib.Config);

  with pLibConfig.Extrato do
  begin
    if GerarPDF or (TipoExtrato = teFortes) then
      ACBrSAT1.Extrato := ACBrSATExtratoFortes1
    else
      ACBrSAT1.Extrato := ACBrSATExtratoESCPOS1;

    pLibConfig.Extrato.Assign(ACBrSAT1.Extrato);

    if NomeImpressora <> '' then
      ACBrSAT1.Extrato.Impressora := NomeImpressora;

    if GerarPDF then
      ACBrSAT1.Extrato.Filtro := fiPDF
  end;
end;

procedure TLibSatDM.CarregarDadosVenda(XmlArquivoOuString: String; aNomePDF: String);
begin
  if Trim(XmlArquivoOuString) = '' then exit;

  ACBrSAT1.CFe.Clear;

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

  if Assigned(ACBrSAT1.Extrato) and (ACBrSAT1.Extrato.Filtro = fiPDF) then
      ACBrSAT1.Extrato.NomeDocumento := IfThen(aNomePDF <> '', aNomePDF ,
        ACBrSAT1.CalcCFeNomeArq(ACBrSAT1.ConfigArquivos.PastaCFeVenda, ACBrSAT1.CFe.infCFe.ID,'','.pdf'));
end;

procedure TLibSatDM.CarregarDadosCancelamento(aStr: String; aNomePDF: String);
begin
  if Trim(aStr) = '' then exit;

  if FileExists(aStr) then
  begin
    GravarLog('Carregando arquivo xml cancelamento [' + aStr + ']', logParanoico);
    ACBrSAT1.CFeCanc.LoadFromFile(aStr);
  end
  else
  begin
    GravarLog('Carregando xml string de cancelamento  [' + aStr + ']', logParanoico);
    ACBrSAT1.CFeCanc.AsXMLString := aStr;
  end;

  if Assigned(ACBrSAT1.Extrato) and (ACBrSAT1.Extrato.Filtro = fiPDF) then
      ACBrSAT1.Extrato.NomeDocumento := IfThen(aNomePDF <> '', aNomePDF ,
        ACBrSAT1.CalcCFeNomeArq(ACBrSAT1.ConfigArquivos.PastaCFeCancelamento, ACBrSAT1.CFeCanc.infCFe.ID,'','.pdf'));
end;

procedure TLibSatDM.Travar;
begin
  GravarLog('Travar', logParanoico);
  FLock.Acquire;
end;

procedure TLibSatDM.Destravar;
begin
  GravarLog('Destravar', logParanoico);
  FLock.Release;
end;

function TLibSatDM.RespostaIntegrador: String;
Var
  Resp: TIntegradorResp;
begin
  Result := '';
  if ACBrSAT1.Integrador = ACBrIntegrador1 then
  begin
    Resp := TIntegradorResp.Create(pLib.Config.TipoResposta, pLib.Config.FormatoResposta);
    try
      Resp.Processar(ACBrIntegrador1);
      Result := sLineBreak + Resp.Gerar;
    finally
      Resp.Free;
    end;
  end;
end;

end.

