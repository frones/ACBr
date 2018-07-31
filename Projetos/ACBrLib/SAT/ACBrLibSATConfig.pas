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

unit ACBrLibSATConfig;

interface

uses
  Classes, SysUtils, IniFiles,
  ACBrLibConfig, ACBrSAT, ACBrSATClass,
  pcnRede, ACBrIntegradorConfig;

type
  TTipoExtrato = (teFortes, teEscPos);

  { TExtratoConfig }
  TExtratoConfig = class
  private
    FTipoExtrato: TTipoExtrato;

  public
    constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property TipoExtrato: TTipoExtrato read FTipoExtrato write FTipoExtrato;

  end;

  { TLibSATConfig }
  TLibSATConfig = class(TLibConfig)
  private
    FModelo: TACBrSATModelo;
    FNomeDLL: string;
    FValidarNumeroSessaoResposta: boolean;
    FNumeroTentativasValidarSessao: integer;
    FArqLOG: string;
    FMFe: boolean;
    FConfig: TACBrSATConfig;
    FConfigArquivos: TACBrSATConfigArquivos;
    FRede: TRede;
    FExtrato: TExtratoConfig;
    FIntegrador: TIntegradorConfig;

  protected
    function AtualizarArquivoConfiguracao: boolean; override;

    procedure INIParaClasse; override;
    procedure ClasseParaINI; override;
    procedure ClasseParaComponentes; override;

    procedure Travar; override;
    procedure Destravar; override;

  public
    constructor Create(AOwner: TObject; ANomeArquivo: string = '';
      AChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property Modelo: TACBrSATModelo read FModelo write FModelo;
    property NomeDLL: string read FNomeDLL write FNomeDLL;
    property ValidarNumeroSessaoResposta: boolean
      read FValidarNumeroSessaoResposta write FValidarNumeroSessaoResposta;
    property NumeroTentativasValidarSessao: integer
      read FNumeroTentativasValidarSessao write FNumeroTentativasValidarSessao;
    property ArqLOG: string read FArqLOG write FArqLOG;
    property IsMFe: boolean read FMFe write FMFe;
    property Config : TACBrSATConfig read FConfig;
    property ConfigArquivos : TACBrSATConfigArquivos read FConfigArquivos;
    property Rede : TRede read FRede;
    property Extrato : TExtratoConfig read FExtrato;
    property Integrador : TIntegradorConfig read FIntegrador;

  end;

implementation

uses
  ACBrLibSATClass, ACBrLibSATConsts, ACBrLibConsts, ACBrLibComum,
  ACBrUtil, ACBrDFeSSL, pcnConversao;

{ TExtratoConfig }
constructor TExtratoConfig.Create;
begin
  FTipoExtrato := teFortes;
end;

procedure TExtratoConfig.LerIni(const AIni: TCustomIniFile);
begin
  FTipoExtrato := TTipoExtrato(AIni.ReadInteger(CSessaoExtrato, CChaveTipo, Integer(FTipoExtrato)));
end;

procedure TExtratoConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteInteger(CSessaoExtrato, CChaveTipo, Integer(FTipoExtrato));
end;

{ TLibSATConfig }

constructor TLibSATConfig.Create(AOwner: TObject; ANomeArquivo: string;
  AChaveCrypt: ansistring);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);

  FModelo := satNenhum;
  FNomeDLL := '';
  FArqLOG := '' ;
  FValidarNumeroSessaoResposta := False;
  FNumeroTentativasValidarSessao := CMAX_ERROS_SESSAO;

  FConfig := TACBrSATConfig.Create(nil);
  FConfigArquivos := TACBrSATConfigArquivos.Create(nil);
  FRede := TRede.Create;
  FExtrato := TExtratoConfig.Create;
  FIntegrador := TIntegradorConfig.Create;
end;

destructor TLibSATConfig.Destroy;
begin
  FConfig.Free;
  FConfigArquivos.Free;
  FRede.Free;
  FExtrato.Free;

  inherited Destroy;
end;

procedure TLibSATConfig.LerIni(const AIni: TCustomIniFile);
begin
  FModelo := TACBrSATModelo(AIni.ReadInteger(CSessaoSAT, CChaveModelo, Integer(FModelo)));
  FNomeDLL := AIni.ReadString(CSessaoSAT, CChaveModelo, FNomeDLL);
  FValidarNumeroSessaoResposta := AIni.ReadBool(CSessaoSAT, CChaveValidarNumero, FValidarNumeroSessaoResposta);
  FNumeroTentativasValidarSessao := AIni.ReadInteger(CSessaoSAT, CChaveNumeroTentativas, FNumeroTentativasValidarSessao);
  FArqLog := AIni.ReadString(CSessaoSAT, CChaveArqLog, FArqLog);
  FMFe := AIni.ReadBool(CSessaoSAT, CChaveMFe, FMFe);

  with FConfig do
  begin
    infCFe_versaoDadosEnt := AIni.ReadFloat(CSessaoSATConfig, CChaveVersaoDadosEnt, infCFe_versaoDadosEnt);
    ide_CNPJ := AIni.ReadString(CSessaoSATConfig, CChaveIdeCNPJ, ide_CNPJ);
    ide_numeroCaixa := AIni.ReadInteger(CSessaoSATConfig, CChaveIdeNumeroCaixa, ide_numeroCaixa);
    ide_tpAmb := TpcnTipoAmbiente(AIni.ReadInteger(CSessaoSATConfig, CChaveIdeTpAmb, Integer(ide_tpAmb)));
    emit_CNPJ := AIni.ReadString(CSessaoSATConfig, CChaveEmitCNPJ, emit_CNPJ);
    emit_IE := AIni.ReadString(CSessaoSATConfig, CChaveEmitIE, emit_IE);
    emit_IM := AIni.ReadString(CSessaoSATConfig, CChaveEmitIM, emit_IM);
    emit_cRegTrib := TpcnRegTrib(AIni.ReadInteger(CSessaoSATConfig, CChaveEmitcRegTrib, Integer(emit_cRegTrib)));
    emit_cRegTribISSQN := TpcnRegTribISSQN(AIni.ReadInteger(CSessaoSATConfig, CChaveEmitcRegTribISSQN, Integer(emit_cRegTribISSQN)));
    emit_indRatISSQN := TpcnindRatISSQN(AIni.ReadInteger(CSessaoSATConfig, CChaveEmitIndRatISSQN, Integer(emit_indRatISSQN)));
    EhUTF8 := AIni.ReadBool(CSessaoSATConfig, CChaveEhUTF8, EhUTF8);
    PaginaDeCodigo := AIni.ReadInteger(CSessaoSATConfig, CChavePaginaDeCodigo, PaginaDeCodigo);
    ArqSchema:= AIni.ReadString(CSessaoSATConfig, CChaveArqSchema, ArqSchema);
    XmlSignLib := TSSLXmlSignLib(AIni.ReadInteger(CSessaoSATConfig, CChaveXmlSignLib, Integer(XmlSignLib)));
  end;

  with FConfigArquivos do
  begin
    SalvarCFe := AIni.ReadBool(CSessaoSATConfigArquivos, CChaveSalvarCFe, SalvarCFe);
    SalvarCFeCanc := AIni.ReadBool(CSessaoSATConfigArquivos, CChaveSalvarCFeCanc, SalvarCFeCanc);
    SalvarEnvio := AIni.ReadBool(CSessaoSATConfigArquivos, CChaveSalvarEnvio, SalvarEnvio);
    SepararPorCNPJ := AIni.ReadBool(CSessaoSATConfigArquivos, CChaveSepararPorCNPJ, SepararPorCNPJ);
    SepararPorModelo := AIni.ReadBool(CSessaoSATConfigArquivos, CChaveSepararPorModelo, SepararPorModelo);
    SepararPorAno := AIni.ReadBool(CSessaoSATConfigArquivos, CChaveSepararPorAno, SepararPorAno);
    SepararPorMes := AIni.ReadBool(CSessaoSATConfigArquivos, CChaveSepararPorMes, SepararPorMes);
    SepararPorDia := AIni.ReadBool(CSessaoSATConfigArquivos, CChaveSepararPorDia, SepararPorDia);
    PastaCFeVenda := AIni.ReadString(CSessaoSATConfigArquivos, CChavePastaCFeVenda, PastaCFeVenda);
    PastaCFeCancelamento := AIni.ReadString(CSessaoSATConfigArquivos, CChavePastaCFeCancelamento, PastaCFeCancelamento);
    PastaEnvio := AIni.ReadString(CSessaoSATConfigArquivos, CChavePastaEnvio, PastaEnvio);
    PrefixoArqCFe := AIni.ReadString(CSessaoSATConfigArquivos, CChavePrefixoArqCFe, PrefixoArqCFe);
    PrefixoArqCFeCanc := AIni.ReadString(CSessaoSATConfigArquivos, CChavePrefixoArqCFeCanc, PrefixoArqCFeCanc);
  end;

  with FRede do
  begin
    tipoInter := TTipoInterface(AIni.ReadInteger(CSessaoSATRede, CChaveTipoInter, Integer(tipoInter)));
    SSID := AIni.ReadString(CSessaoSATRede, CChaveSSID, SSID);
    seg := TSegSemFio(AIni.ReadInteger(CSessaoSATRede, CChaveSeg, Integer(seg)));
    codigo := AIni.ReadString(CSessaoSATRede, CChaveCodigo, codigo);
    tipoLan := TTipoLan(AIni.ReadInteger(CSessaoSATRede, CChaveTipoLan, Integer(tipoLan)));
    lanIP := AIni.ReadString(CSessaoSATRede, CChaveLanIP, lanIP);
    lanMask := AIni.ReadString(CSessaoSATRede, CChaveLanMask, lanMask);
    lanGW := AIni.ReadString(CSessaoSATRede, CChaveLanGW, lanGW);
    lanDNS1 := AIni.ReadString(CSessaoSATRede, CChaveLanDNS1, lanDNS1);
    lanDNS2 := AIni.ReadString(CSessaoSATRede, CChaveLanDNS2, lanDNS2);
    usuario := AIni.ReadString(CSessaoSATRede, CChaveUsuario, usuario);
    senha := AIni.ReadString(CSessaoSATRede, CChaveSenha, '');
    proxy := AIni.ReadInteger(CSessaoSATRede, CChaveProxy, proxy);
    proxy_ip := AIni.ReadString(CSessaoSATRede, CChaveProxyIp, proxy_ip);
    proxy_porta := AIni.ReadInteger(CSessaoSATRede, CChaveProxyPorta, proxy_porta);
    proxy_user := AIni.ReadString(CSessaoSATRede, CChaveProxyUser, proxy_user);
    proxy_senha := AIni.ReadString(CSessaoSATRede, CChaveProxySenha, '');
  end;

  FExtrato.LerIni(AIni);
  FIntegrador.LerIni(AIni);
end;

procedure TLibSATConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteInteger(CSessaoSAT, CChaveModelo, Integer(FModelo));
  AIni.WriteString(CSessaoSAT, CChaveModelo, FNomeDLL);
  AIni.WriteBool(CSessaoSAT, CChaveValidarNumero, FValidarNumeroSessaoResposta);
  AIni.WriteInteger(CSessaoSAT, CChaveNumeroTentativas, FNumeroTentativasValidarSessao);
  AIni.WriteString(CSessaoSAT, CChaveArqLog, FArqLog);
  AIni.WriteBool(CSessaoSAT, CChaveMFe, FMFe);

  with FConfig do
  begin
    AIni.WriteFloat(CSessaoSATConfig, CChaveVersaoDadosEnt, infCFe_versaoDadosEnt);
    AIni.WriteString(CSessaoSATConfig, CChaveIdeCNPJ, ide_CNPJ);
    AIni.WriteInteger(CSessaoSATConfig, CChaveIdeNumeroCaixa, ide_numeroCaixa);
    AIni.WriteInteger(CSessaoSATConfig, CChaveIdeTpAmb, Integer(ide_tpAmb));
    AIni.WriteString(CSessaoSATConfig, CChaveEmitCNPJ, emit_CNPJ);
    AIni.WriteString(CSessaoSATConfig, CChaveEmitIE, emit_IE);
    AIni.WriteString(CSessaoSATConfig, CChaveEmitIM, emit_IM);
    AIni.WriteInteger(CSessaoSATConfig, CChaveEmitcRegTrib, Integer(emit_cRegTrib));
    AIni.WriteInteger(CSessaoSATConfig, CChaveEmitcRegTribISSQN, Integer(emit_cRegTribISSQN));
    AIni.WriteInteger(CSessaoSATConfig, CChaveEmitIndRatISSQN, Integer(emit_indRatISSQN));
    AIni.WriteBool(CSessaoSATConfig, CChaveEhUTF8, EhUTF8);
    AIni.WriteInteger(CSessaoSATConfig, CChavePaginaDeCodigo, PaginaDeCodigo);
    AIni.WriteString(CSessaoSATConfig, CChaveArqSchema, ArqSchema);
    AIni.WriteInteger(CSessaoSATConfig, CChaveXmlSignLib, Integer(XmlSignLib));
  end;

  with FConfigArquivos do
  begin
    AIni.WriteBool(CSessaoSATConfigArquivos, CChaveSalvarCFe, SalvarCFe);
    AIni.WriteBool(CSessaoSATConfigArquivos, CChaveSalvarCFeCanc, SalvarCFeCanc);
    AIni.WriteBool(CSessaoSATConfigArquivos, CChaveSalvarEnvio, SalvarEnvio);
    AIni.WriteBool(CSessaoSATConfigArquivos, CChaveSepararPorCNPJ, SepararPorCNPJ);
    AIni.WriteBool(CSessaoSATConfigArquivos, CChaveSepararPorModelo, SepararPorModelo);
    AIni.WriteBool(CSessaoSATConfigArquivos, CChaveSepararPorAno, SepararPorAno);
    AIni.WriteBool(CSessaoSATConfigArquivos, CChaveSepararPorMes, SepararPorMes);
    AIni.WriteBool(CSessaoSATConfigArquivos, CChaveSepararPorDia, SepararPorDia);
    AIni.WriteString(CSessaoSATConfigArquivos, CChavePastaCFeVenda, PastaCFeVenda);
    AIni.WriteString(CSessaoSATConfigArquivos, CChavePastaCFeCancelamento, PastaCFeCancelamento);
    AIni.WriteString(CSessaoSATConfigArquivos, CChavePastaEnvio, PastaEnvio);
    AIni.WriteString(CSessaoSATConfigArquivos, CChavePrefixoArqCFe, PrefixoArqCFe);
    AIni.WriteString(CSessaoSATConfigArquivos, CChavePrefixoArqCFeCanc, PrefixoArqCFeCanc);
  end;

  with FRede do
  begin
    AIni.WriteInteger(CSessaoSATRede, CChaveTipoInter, Integer(tipoInter));
    AIni.WriteString(CSessaoSATRede, CChaveSSID, SSID);
    AIni.WriteInteger(CSessaoSATRede, CChaveSeg, Integer(seg));
    AIni.WriteString(CSessaoSATRede, CChaveCodigo, codigo);
    AIni.WriteInteger(CSessaoSATRede, CChaveTipoLan, Integer(tipoLan));
    AIni.WriteString(CSessaoSATRede, CChaveLanIP, lanIP);
    AIni.WriteString(CSessaoSATRede, CChaveLanMask, lanMask);
    AIni.WriteString(CSessaoSATRede, CChaveLanGW, lanGW);
    AIni.WriteString(CSessaoSATRede, CChaveLanDNS1, lanDNS1);
    AIni.WriteString(CSessaoSATRede, CChaveLanDNS2, lanDNS2);
    AIni.WriteString(CSessaoSATRede, CChaveUsuario, usuario);
    AIni.WriteString(CSessaoSATRede, CChaveSenha, StringToB64Crypt(senha, FChaveCrypt));
    AIni.WriteInteger(CSessaoSATRede, CChaveProxy, proxy);
    AIni.WriteString(CSessaoSATRede, CChaveProxyIp, proxy_ip);
    AIni.WriteInteger(CSessaoSATRede, CChaveProxyPorta, proxy_porta);
    AIni.WriteString(CSessaoSATRede, CChaveProxyUser, proxy_user);
    AIni.WriteString(CSessaoSATRede, CChaveProxySenha, StringToB64Crypt(proxy_senha, FChaveCrypt));
  end;

  FExtrato.GravarIni(AIni);
  FIntegrador.GravarIni(AIni);
end;

function TLibSATConfig.AtualizarArquivoConfiguracao: boolean;
var
  Versao: string;
begin
  Versao := Ini.ReadString(CSessaoVersao, CLibSATNome, '0');
  Result := (CompareVersions(CLibSATVersao, Versao) > 0) or
    (inherited AtualizarArquivoConfiguracao);
end;

procedure TLibSATConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  LerIni(Ini);
end;

procedure TLibSATConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  Ini.WriteString(CSessaoVersao, CLibSATNome, CLibSATVersao);

  GravarIni(Ini);
end;

procedure TLibSATConfig.ClasseParaComponentes;
begin
  if Assigned(Owner) then
    TACBrLibSAT(Owner).SATDM.AplicarConfiguracoes;
end;

procedure TLibSATConfig.Travar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibSAT(Owner) do
      SATDM.Travar;
  end;
end;

procedure TLibSATConfig.Destravar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibSAT(Owner) do
      SATDM.Destravar;
  end;
end;

end.
