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
  Classes, SysUtils, FileUtil, syncobjs, ACBrLibConfig, ACBrSAT, ACBrIntegrador,
  ACBrSATExtratoESCPOS, ACBrSATExtratoFortesFr, ACBrECFVirtualSAT, ACBrSATWS;

type

  { TLibSatDM }

  TLibSatDM = class(TDataModule)
    ACBrIntegrador1: TACBrIntegrador;
    ACBrSAT1: TACBrSAT;
    ACBrSATExtratoESCPOS1: TACBrSATExtratoESCPOS;
    ACBrSATExtratoFortes1: TACBrSATExtratoFortes;
    ACBrSATWS1: TACBrSATWS;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FLock: TCriticalSection;

  public
    procedure AplicarConfiguracoes;
    procedure GravarLog(AMsg: String; NivelLog: TNivelLog; Traduzir: Boolean = False);
    procedure Travar;
    procedure Destravar;

  end;

implementation

uses
  ACBrUtil,
  ACBrLibSATConfig, ACBrLibComum, ACBrLibSATClass;

{$R *.lfm}

{ TLibSatDM }

procedure TLibSatDM.DataModuleCreate(Sender: TObject);
begin
  FLock := TCriticalSection.Create;
end;

procedure TLibSatDM.DataModuleDestroy(Sender: TObject);
begin
  FLock.Destroy;
end;

procedure TLibSatDM.AplicarConfiguracoes;
var
  pLibConfig: TLibSATConfig;
begin
  pLibConfig := TLibSATConfig(TACBrLibSAT(pLib).Config);

  with ACBrSAT1 do
  begin
    Modelo := pLibConfig.Modelo;
    NomeDLL := pLibConfig.NomeDLL;
    ValidarNumeroSessaoResposta := pLibConfig.ValidarNumeroSessaoResposta;
    NumeroTentativasValidarSessao := pLibConfig.NumeroTentativasValidarSessao;
    ArqLOG := pLibConfig.ArqLOG;

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

    with ConfigArquivos do
    begin
      SalvarCFe := pLibConfig.ConfigArquivos.SalvarCFe;
      SalvarCFeCanc := pLibConfig.ConfigArquivos.SalvarCFeCanc;
      SalvarEnvio := pLibConfig.ConfigArquivos.SalvarEnvio;
      SepararPorCNPJ := pLibConfig.ConfigArquivos.SepararPorCNPJ;
      SepararPorModelo := pLibConfig.ConfigArquivos.SepararPorModelo;
      SepararPorAno := pLibConfig.ConfigArquivos.SepararPorAno;
      SepararPorMes := pLibConfig.ConfigArquivos.SepararPorMes;
      SepararPorDia := pLibConfig.ConfigArquivos.SepararPorDia;
      PastaCFeVenda := pLibConfig.ConfigArquivos.PastaCFeVenda;
      PastaCFeCancelamento := pLibConfig.ConfigArquivos.PastaCFeCancelamento;
      PastaEnvio := pLibConfig.ConfigArquivos.PastaEnvio;
      PrefixoArqCFe := pLibConfig.ConfigArquivos.PrefixoArqCFe;
      PrefixoArqCFeCanc := pLibConfig.ConfigArquivos.PrefixoArqCFeCanc;
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
      senha := pLibConfig.Rede.senha;
      proxy := pLibConfig.Rede.proxy;
      proxy_ip := pLibConfig.Rede.proxy_ip;
      proxy_porta := pLibConfig.Rede.proxy_porta;
      proxy_user := pLibConfig.Rede.proxy_user;
      proxy_senha := pLibConfig.Rede.proxy_senha;
    end;

    if pLibConfig.Extrato.TipoExtrato = teFortes then
      Extrato := ACBrSATExtratoFortes1
    else
      Extrato := ACBrSATExtratoESCPOS1;

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
  end;
end;

procedure TLibSatDM.GravarLog(AMsg: String; NivelLog: TNivelLog;
  Traduzir: Boolean);
begin
  if Assigned(pLib) then
    pLib.GravarLog(AMsg, NivelLog, Traduzir);
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

end.

