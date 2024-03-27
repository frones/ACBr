{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: André Ferreira de Moraes                        }
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

unit ACBrSAT;

interface

uses
  Classes, SysUtils, pcnCFe, pcnRede, pcnCFeCanc, ACBrBase, ACBrSATClass,
  ACBrSATExtratoClass, synacode, ACBrMail, ACBrIntegrador, ACBrDFeSSL;

const
  CPREFIXO_CFe = 'CFe';
  CMAX_ERROS_SESSAO = 1;

type

   TACBrSATEvento = procedure(var ARetorno: String) of object;
   TACBrSATEventoDados = procedure (ADados: String; var ARetorno: String) of object;
   TACBrSATMensagem = procedure ( ACod: Integer; AMensagem: String) of object;
   TACBrSATCalcPathEvent = procedure (var APath: String; ACNPJ: String; AData: TDateTime) of object;

   { TACBrSAT }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
   TACBrSAT = class( TACBrComponent )
   private
     fsCFe : TCFe ;
     fsCFeCanc : TCFeCanc ;
     fsnumeroSessao : Integer ;
     fsAguardandoResposta: Boolean;
     fsOnAguardandoRespostaChange: TNotifyEvent;
     fsOnGetcodigoDeAtivacao : TACBrSATGetChave ;
     fsOnGetNumeroSessao: TACBrSATGetNumeroSessao;
     fsOnGetsignAC : TACBrSATGetChave ;
     fsOnGravarLog : TACBrGravarLog ;
     fsNomeDLL : String ;
     fsPrefixoCFe: String;
     fsResposta : TACBrSATResposta ;
     fsRespostaComando : String ;
     fsSATClass : TACBrSATClass ;
     fsExtrato : TACBrSATExtratoClass;
     fsMAIL: TACBrMail;
     fsIntegrador: TACBrIntegrador;
     fsSSL: TDFeSSL;

     fsArqLOG: String;
     fsComandoLog: String;
     fsInicializado : Boolean ;
     fsModelo : TACBrSATModelo ;
     fsConfig : TACBrSATConfig ;
     fsConfigArquivos : TACBrSATConfigArquivos ;
     fsRede   : TRede ;
     fsStatus : TACBrSATStatus;

     fsOnEnviarDadosVenda: TACBrSATEventoDados;
     fsOnCancelarUltimaVenda: TACBrSATEventoDados;
     fsOnConsultaStatusOperacional: TACBrSATEvento;
     fsOnExtrairLogs: TACBrSATEvento;
     fsOnConsultarSAT: TACBrSATEvento;
     fsOnConsultarNumeroSessao: TACBrSATEventoDados;
     fsOnMensagemSEFAZ: TACBrSATMensagem;
     fsOnCalcPath: TACBrSATCalcPathEvent;
     fsOnConsultarUltimaSessaoFiscal: TACBrSATEvento;

     fsValidarNumeroSessaoResposta: Boolean;
     fsNumeroTentativasValidarSessao: Integer;
     fsErrosSessaoCount: Integer;
     fsSessaoAVerificar: Integer;
     // Italo
     fsLoteCFe: TLoteCFeCollection;
     fsLoteCFeCanc: TLoteCFeCancCollection;

     function CodificarPaginaDeCodigoSAT(const ATexto: String): AnsiString;
     function DecodificarPaginaDeCodigoSAT(const ATexto: AnsiString): String;

     function GetcodigoDeAtivacao : AnsiString ;
     function GetModeloStrClass : String ;
     function GetsignAC : AnsiString ;
     procedure SetInicializado(AValue : Boolean) ;
     procedure SetModelo(AValue : TACBrSATModelo) ;
     procedure SetNomeDLL(const AValue : string) ;
     procedure SetAguardandoResposta(AValue: Boolean);

     procedure VerificaInicializado ;
     procedure VerificaCondicoesImpressao( EhCancelamento: Boolean = False);

     procedure GravaLog(const AString : AnsiString ) ;
     procedure SetExtrato(const Value: TACBrSATExtratoClass);
     procedure SetMAIL(const AValue: TACBrMail);
     procedure SetIntegrador(AValue: TACBrIntegrador);

     function GravarStream(AStream: TStream): Boolean;
   protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
   public
     procedure IniciaComando ;
     function FinalizaComando(const AResult: String): String;
     procedure DecodificaRetorno6000;
     procedure DecodificaRetorno7000;
     property SAT : TACBrSATClass read fsSATClass ;
     property SSL: TDFeSSL read fsSSL;

     constructor Create( AOwner : TComponent ) ; override;
     destructor Destroy ; override;

     function GetNomeModeloCFe: String;

     function LerLoteCFe(const AFileName: String; AGravarXml: Boolean = True): boolean;

     Procedure Inicializar;
     Procedure DesInicializar;
     property Inicializado : Boolean read fsInicializado write SetInicializado ;

     Property ModeloStr : String  read GetModeloStrClass;

     property numeroSessao : Integer read fsnumeroSessao;
     function GerarnumeroSessao : Integer ;

     property codigoDeAtivacao : AnsiString read GetcodigoDeAtivacao ;
     property signAC           : AnsiString read GetsignAC ;

     property RespostaComando: String read fsRespostaComando ;
     Property AguardandoResposta : Boolean read fsAguardandoResposta ;

     property CFe : TCFe read fsCFe ;
     property CFeCanc : TCFeCanc read fsCFeCanc ;
     // Italo
     property LoteCFe: TLoteCFeCollection read fsLoteCFe;
     property LoteCFeCanc: TLoteCFeCancCollection read fsLoteCFeCanc;

     property Status : TACBrSATStatus read fsStatus;
     property Resposta : TACBrSATResposta read fsResposta;
     property PrefixoCFe: String read fsPrefixoCFe;

     procedure InicializaCFe( ACFe : TCFe = nil );
     function VerificarVersaoSAT(const VersaoLayout: Double = 0): Double;

     procedure DoLog(const AString : String ) ;

     function AssociarAssinatura( CNPJvalue: AnsiString; const assinaturaCNPJs : AnsiString ): String ;
     function AtivarSAT(subComando : Integer ; CNPJ : AnsiString ; cUF : Integer
       ) : String ;
     function AtualizarSoftwareSAT : String ;
     function BloquearSAT : String ;
     procedure CFe2CFeCanc;
     function CancelarUltimaVenda :String ; overload;
     function CancelarUltimaVenda( chave, dadosCancelamento : AnsiString ) :
       String ; overload;
     function ComunicarCertificadoICPBRASIL( const certificado : AnsiString ) :
       String ;
     function ConfigurarInterfaceDeRede( dadosConfiguracao : AnsiString = '') :
       String ;
     function ConsultarNumeroSessao( cNumeroDeSessao : Integer) : String ;
     function ConsultarUltimaSessaoFiscal : String ;
     function ConsultarSAT : String ;
     function ConsultarStatusOperacional : String ;
     function DesbloquearSAT : String ;
     function EnviarDadosVenda : String ; overload;
     function EnviarDadosVenda( dadosVenda : AnsiString ) : String ; overload;
     procedure ExtrairLogs( const NomeArquivo : String ); overload;
     procedure ExtrairLogs( AStringList : TStrings ); overload;
     procedure ExtrairLogs( AStream : TStream ); overload;
     function TesteFimAFim( const dadosVenda : AnsiString) : String ;
     function TrocarCodigoDeAtivacao(const codigoDeAtivacaoOuEmergencia: AnsiString;
       opcao: Integer; const novoCodigo: AnsiString): String;
     function ValidarDadosVenda( dadosVenda : AnsiString; out msgErro: String) : Boolean;

    procedure ImprimirExtrato;
    procedure ImprimirExtratoResumido;
    procedure ImprimirExtratoCancelamento;

    function CalcCFeNomeArq( const Pasta: String; NomeArquivo: String = '';
      const Sufixo: String = ''; const Extensao: String = '.xml'): String;
    function CalcCFeCancNomeArq( const Pasta: String; NomeArquivo: String = '';
      const Sufixo: String = ''; const Extensao: String = '.xml'): String;

    procedure EnviarEmail(const sPara, sAssunto: String; const NomeArq: String = '';
      sMensagem: TStrings = nil; sCC: TStrings = nil; Anexos: TStrings = nil;
      StreamCFe: TStream = nil); overload;

    procedure EnviarEmail(const sPara, sAssunto: String;
      sMensagem: TStrings = nil; sCC: TStrings = nil; Anexos: TStrings = nil); overload;

   published
     property MAIL: TACBrMail read fsMAIL write SetMAIL;
     property Integrador: TACBrIntegrador read fsIntegrador write SetIntegrador;

     property Modelo : TACBrSATModelo read fsModelo write SetModelo
                 default satNenhum ;

     property Extrato: TACBrSATExtratoClass read fsExtrato write SetExtrato;

     property NomeDLL: string read fsNomeDLL write SetNomeDLL;

     property ValidarNumeroSessaoResposta: Boolean read fsValidarNumeroSessaoResposta
       write fsValidarNumeroSessaoResposta default False;
     property NumeroTentativasValidarSessao: Integer read fsNumeroTentativasValidarSessao
       write fsNumeroTentativasValidarSessao default CMAX_ERROS_SESSAO;

     property ArqLOG : String read fsArqLOG write fsArqLOG ;
     property OnGravarLog : TACBrGravarLog read fsOnGravarLog write fsOnGravarLog;

     property Config : TACBrSATConfig read fsConfig write fsConfig;
     property ConfigArquivos : TACBrSATConfigArquivos read fsConfigArquivos
        write fsConfigArquivos;
     property Rede : TRede read fsRede write fsRede;

     property OnGetcodigoDeAtivacao : TACBrSATGetChave read fsOnGetcodigoDeAtivacao
        write fsOnGetcodigoDeAtivacao;
     property OnGetsignAC : TACBrSATGetChave read fsOnGetsignAC write fsOnGetsignAC;
     property OnGetNumeroSessao : TACBrSATGetNumeroSessao read fsOnGetNumeroSessao
        write fsOnGetNumeroSessao;
     property OnAguardandoRespostaChange : TNotifyEvent
        read fsOnAguardandoRespostaChange write fsOnAguardandoRespostaChange ;

     property OnEnviarDadosVenda: TACBrSATEventoDados read fsOnEnviarDadosVenda
        write fsOnEnviarDadosVenda;
     property OnCancelarUltimaVenda :TACBrSATEventoDados read fsOnCancelarUltimaVenda
        write fsOnCancelarUltimaVenda;
     property OnConsultaStatusOperacional: TACBrSATEvento
        read fsOnConsultaStatusOperacional write fsOnConsultaStatusOperacional;
     property OnExtrairLogs: TACBrSATEvento read fsOnExtrairLogs write fsOnExtrairLogs;
     property OnConsultarSAT: TACBrSATEvento read fsOnConsultarSAT write fsOnConsultarSAT;
     property OnConsultarNumeroSessao:TACBrSATEventoDados read fsOnConsultarNumeroSessao
        write fsOnConsultarNumeroSessao;
     property OnMensagemSEFAZ: TACBrSATMensagem read fsOnMensagemSEFAZ
        write fsOnMensagemSEFAZ;
     property OnCalcPath: TACBrSATCalcPathEvent read fsOnCalcPath write fsOnCalcPath;
     property OnConsultarUltimaSessaoFiscal: TACBrSATEvento read fsOnConsultarUltimaSessaoFiscal
        write fsOnConsultarUltimaSessaoFiscal;

end;

    function MensagemCodigoRetorno(CodigoRetorno: Integer): String;
    function MotivocStat(cStat: Integer): String;
    function MotivoInvalidoVenda(cod: integer): String;
    function MotivoInvalidoCancelamento(cod: integer): String;

// SAT ER 2.30.03 //27/03/2024

implementation

Uses
  dateutils,
  ACBrUtil.Strings, ACBrUtil.FilesIO, ACBrUtil.XMLHTML, ACBrUtil.DateTime, ACBrUtil.Base,
  ACBrConsts, ACBrSATDinamico_cdecl, ACBrSATDinamico_stdcall, ACBrSATMFe_integrador,
  synautil;

function MensagemCodigoRetorno(CodigoRetorno: Integer): String;
var
  Mensagem: String;
begin
  (* Retorna a mensagem de erro do código no parametro CodigoRetorno
       de acordo com a NOTA TECNICA 2013/001 *)
  case (CodigoRetorno) of
    04000: Mensagem := 'Ativado corretamente SAT Ativado com Sucesso.';
    04001: Mensagem := 'Erro na criação do certificado processo de ativação foi interrompido.';
    04002: Mensagem := 'SEFAZ não reconhece este SAT (CNPJ inválido) Verificar junto a SEFAZ o CNPJ cadastrado.';
    04003: Mensagem := 'SAT já ativado SAT disponível para uso.';
    04004: Mensagem := 'SAT com uso cessado SAT bloqueado por cessação de uso.';
    04005: Mensagem := 'Erro de comunicação com a SEFAZ Tentar novamente.';
    04006: Mensagem := 'CSR ICP-BRASIL criado com sucesso Processo de criação do CSR para certificação ICP-BRASIL com sucesso';
    04007: Mensagem := 'Erro na criação do CSR ICP-BRASIL Processo de criação do CSR para certificação ICP-BRASIL com erro';
    04098: Mensagem := 'SAT em processamento. Tente novamente.';
    04099: Mensagem := 'Erro desconhecido na ativação Informar ao administrador.';
    05000: Mensagem := 'Certificado transmitido com Sucesso ';
    05001: Mensagem := 'Código de ativação inválido.';
    05002: Mensagem := 'Erro de comunicação com a SEFAZ. Tentar novamente.';
    05003: Mensagem := 'Certificado Inválido ';
    05098: Mensagem := 'SAT em processamento.';
    05099: Mensagem := 'Erro desconhecido Informar o administrador.';
    06000: Mensagem := 'Emitido com sucesso + conteúdo notas. Retorno CF-e-SAT ao AC para contingência.';
    06001: Mensagem := 'Código de ativação inválido.';
    06002: Mensagem := 'SAT ainda não ativado. Efetuar ativação.';
    06003: Mensagem := 'SAT não vinculado ao AC Efetuar vinculação';
    06004: Mensagem := 'Vinculação do AC não confere Efetuar vinculação';
    06005: Mensagem := 'Tamanho do CF-e-SAT superior a 1.500KB';
    06006: Mensagem := 'SAT bloqueado pelo contribuinte';
    06007: Mensagem := 'SAT bloqueado pela SEFAZ';
    06008: Mensagem := 'SAT bloqueado por falta de comunicação';
    06009: Mensagem := 'SAT bloqueado, código de ativação incorreto';
    06010: Mensagem := 'Erro de validação do conteúdo.';
    06098: Mensagem := 'SAT em processamento.';
    06099: Mensagem := 'Erro desconhecido na emissão. Informar o administrador.';
    07000: Mensagem := 'Cupom cancelado com sucesso + conteúdo CF-eSAT cancelado.';
    07001: Mensagem := 'Código ativação inválido Verificar o código e tentar mais uma vez.';
    07002: Mensagem := 'Cupom inválido Informar o administrador.';
    07003: Mensagem := 'SAT bloqueado pelo contribuinte';
    07004: Mensagem := 'SAT bloqueado pela SEFAZ';
    07005: Mensagem := 'SAT bloqueado por falta de comunicação';
    07006: Mensagem := 'SAT bloqueado, código de ativação incorreto';
    07007: Mensagem := 'Erro de validação do conteúdo';
    07098: Mensagem := 'SAT em processamento.';
    07099: Mensagem := 'Erro desconhecido no cancelamento.';
    08000: Mensagem := 'SAT em operação. Verifica se o SAT está ativo.';
    08098: Mensagem := 'SAT em processamento.';
    08099: Mensagem := 'Erro desconhecido. Informar o administrador.';
    09000: Mensagem := 'Emitido com sucesso Gera e envia um cupom de teste para SEFAZ, para verificar a comunicação.';
    09001: Mensagem := 'código ativação inválido Verificar o código e tentar mais uma vez.';
    09002: Mensagem := 'SAT ainda não ativado. Efetuar ativação ';
    09098: Mensagem := 'SAT em processamento.';
    09099: Mensagem := 'Erro desconhecido Informar o ';
    10000: Mensagem := 'Resposta com Sucesso. Informações de status do SAT.';
    10001: Mensagem := 'Código de ativação inválido';
    10098: Mensagem := 'SAT em processamento.';
    10099: Mensagem := 'Erro desconhecido Informar o administrador.';
    11000: Mensagem := 'Emitido com sucesso Retorna o conteúdo do CF-ao AC.';
    11001: Mensagem := 'código ativação inválido Verificar o código e tentar mais uma vez.';
    11002: Mensagem := 'SAT ainda não ativado. Efetuar ativação.';
    11003: Mensagem := 'Sessão não existe. AC deve executar a sessão novamente.';
    11098: Mensagem := 'SAT em processamento.';
    11099: Mensagem := 'Erro desconhecido. Informar o administrador.';
    12000: Mensagem := 'Rede Configurada com Sucesso';
    12001: Mensagem := 'código ativação inválido Verificar o código e tentar mais uma vez.';
    12002: Mensagem := 'Dados fora do padrão a ser informado Corrigir dados';
    12098: Mensagem := 'SAT em processamento.';
    12099: Mensagem := 'Erro desconhecido Informar o administrador.';
    13000: Mensagem := 'Assinatura do AC';
    13001: Mensagem := 'código ativação inválido Verificar o código e tentar mais uma vez.';
    13002: Mensagem := 'Erro de comunicação com a SEFAZ';
    13003: Mensagem := 'Assinatura fora do padrão informado Corrigir dados';
    13004: Mensagem := 'CNPJ da Software House + CNPJ do emitente assinado no campo “signAC” difere do informado no campo “CNPJvalue” Corrigir dados';
    13098: Mensagem := 'SAT em processamento.';
    13099: Mensagem := 'Erro desconhecido Informar o administrador.';
    14000: Mensagem := 'Software Atualizado com Sucesso ';
    14001: Mensagem := 'Código de ativação inválido.';
    14002: Mensagem := 'Atualização em Andamento';
    14003: Mensagem := 'Erro na atualização Não foi possível Atualizar o SAT.';
    14004: Mensagem := 'Arquivo de atualização inválido';
    14098: Mensagem := 'SAT em processamento.';
    14099: Mensagem := 'Erro desconhecido Informar o administrador.';
    15000: Mensagem := 'Transferência completa Arquivos de Logs extraídos';
    15001: Mensagem := 'Código de ativação inválido.';
    15002: Mensagem := 'Transferência em andamento';
    15098: Mensagem := 'SAT em processamento.';
    15099: Mensagem := 'Erro desconhecido Informar o administrador.';
    16000: Mensagem := 'Equipamento SAT bloqueado com sucesso.';
    16001: Mensagem := 'Código de ativação inválido.';
    16002: Mensagem := 'Equipamento SAT já está bloqueado.';
    16003: Mensagem := 'Erro de comunicação com a SEFAZ';
    16004: Mensagem := 'Não existe parametrização de bloqueio disponível.';
    16098: Mensagem := 'SAT em processamento.';
    16099: Mensagem := 'Erro desconhecido Informar o administrador.';
    17000: Mensagem := 'Equipamento SAT desbloqueado com sucesso.';
    17001: Mensagem := 'Código de ativação inválido.';
    17002: Mensagem := 'SAT bloqueado pelo contribuinte. Verifique configurações na SEFAZ';
    17003: Mensagem := 'SAT bloqueado pela SEFAZ';
    17004: Mensagem := 'Erro de comunicação com a SEFAZ';
    17098: Mensagem := 'SAT em processamento.';
    17099: Mensagem := 'Erro desconhecido Informar o administrador.';
    18000: Mensagem := 'Código de ativação alterado com sucesso.';
    18001: Mensagem := 'Código de ativação inválido.';
    18002: Mensagem := 'Código de ativação de emergência Incorreto.';
    18098: Mensagem := 'SAT em processamento.';
    18099: Mensagem := 'Erro desconhecido Informar o administrador.';
  else
    Mensagem := '';
  end;

  Result := ACBrStr(Mensagem);
end;

function MotivocStat(cStat: Integer): String;
var
  xMotivo: String;
begin
  (* Retorna a mensagem de rejeição do código no parametro
       pCodigo de acordo com a NOTA TECNICA 2013/001 *)

  case (cStat) of
    100: xMotivo := 'CF-e-SAT processado com sucesso';
    101: xMotivo := 'CF-e-SAT de cancelamento processado com sucesso';
    102: xMotivo := 'CF-e-SAT processado – verificar inconsistências';
    103: xMotivo := 'CF-e-SAT de cancelamento processado – verificar inconsistências';
    104: xMotivo := 'Não Existe Atualização do Software';
    105: xMotivo := 'Lote recebido com sucesso';
    106: xMotivo := 'Lote Processado';
    107: xMotivo := 'Lote em Processamento';
    108: xMotivo := 'Lote não localizado';
    109: xMotivo := 'Serviço em Operação';
    110: xMotivo := 'Status SAT recebido com sucesso';
    112: xMotivo := 'Assinatura do AC Registrada';
    113: xMotivo := 'Consulta cadastro com uma ocorrência';
    114: xMotivo := 'Consulta cadastro com mais de uma ocorrência';
    115: xMotivo := 'Solicitação de dados efetuada com sucesso';
    116: xMotivo := 'Atualização do SB pendente';
    117: xMotivo := 'Solicitação de Arquivo de Parametrização efetuada com sucesso';
    118: xMotivo := 'Logs extraídos com sucesso';
    119: xMotivo := 'Comandos da SEFAZ pendentes';
    120: xMotivo := 'Não existem comandos da SEFAZ pendentes';
    121: xMotivo := 'Certificado Digital criado com sucesso';
    122: xMotivo := 'CRT recebido com sucesso';
    123: xMotivo := 'Adiar transmissão do lote';
    124: xMotivo := 'Adiar transmissão do CF-e';
    125: xMotivo := 'CF-e de teste de produção emitido com sucesso';
    126: xMotivo := 'CF-e de teste de ativação emitido com sucesso';
    127: xMotivo := 'Erro na emissão de CF-e de teste de produção';
    128: xMotivo := 'Erro na emissão de CF-e de teste de ativação';
    129: xMotivo := 'Solicitações de emissão de certificados excedidas. (Somente ocorrerá no ambiente de testes)';
    200: xMotivo := 'Rejeição: Status do equipamento SAT difere do esperado';
    201: xMotivo := 'Rejeição: Falha na Verificação da Assinatura do Número de segurança';
    202: xMotivo := 'Rejeição: Falha no reconhecimento da autoria ou integridade do arquivo digital';
    203: xMotivo := 'Rejeição: Emissor não Autorizado para emissão da CF-e-SAT';
    204: xMotivo := 'Rejeição: Duplicidade de CF-e-SAT';
    205: xMotivo := 'Rejeição: Equipamento SAT encontra-se Ativo';
    206: xMotivo := 'Rejeição: Hora de Emissão do CF-e-SAT posterior à hora de recebimento.';
    207: xMotivo := 'Rejeição: CNPJ do emitente inválido';
    208: xMotivo := 'Rejeição: Equipamento SAT encontra-se Desativado';
    209: xMotivo := 'Rejeição: IE do emitente inválida';
    210: xMotivo := 'Rejeição: Intervalo de tempo entre o CF-e-SAT emitido e a emissão do respectivo CF-e-SAT de cancelamento é maior que 30 (trinta) minutos.';
    211: xMotivo := 'Rejeição: CNPJ não corresponde ao informado no processo de transferência.';
    212: xMotivo := 'Rejeição: Data de Emissão do CF-e-SAT posterior à data de recebimento.';
    213: xMotivo := 'Rejeição: CNPJ-Base do Emitente difere do CNPJ-Base do Certificado Digital';
    214: xMotivo := 'Rejeição: Tamanho da mensagem excedeu o limite estabelecido';
    215: xMotivo := 'Rejeição: Falha no schema XML';
    216: xMotivo := 'Rejeição: Chave de Acesso difere da cadastrada';
    217: xMotivo := 'Rejeição: CF-e-SAT não consta na base de dados da SEFAZ';
    218: xMotivo := 'Rejeição: CF-e-SAT já esta cancelado na base de dados da SEFAZ';
    219: xMotivo := 'Rejeição: CNPJ não corresponde ao informado no processo de declaração de posse.';
    220: xMotivo := 'Rejeição: Valor do rateio do desconto sobre subtotal do item (N) inválido.';
    221: xMotivo := 'Rejeição: Aplicativo Comercial não vinculado ao SAT';
    222: xMotivo := 'Rejeição: Assinatura do Aplicativo Comercial inválida';
    223: xMotivo := 'Rejeição: CNPJ do transmissor do lote difere do CNPJ do transmissor da consulta';
    224: xMotivo := 'Rejeição: CNPJ da Software House inválido';
    225: xMotivo := 'Rejeição: Falha no Schema XML do lote de CFe';
    226: xMotivo := 'Rejeição: Código da UF do Emitente diverge da UF receptora';
    227: xMotivo := 'Rejeição: Erro na Chave de Acesso - Campo Id – falta a literal CFe';
    228: xMotivo := 'Rejeição: Valor do rateio do acréscimo sobre subtotal do item (N) inválido.';
    229: xMotivo := 'Rejeição: IE do emitente não informada';
    230: xMotivo := 'Rejeição: IE do emitente não autorizada para uso do SAT';
    231: xMotivo := 'Rejeição: IE do emitente não vinculada ao CNPJ';
    232: xMotivo := 'Rejeição: CNPJ do destinatário do CF-e-SAT de cancelamento diferente daquele do CF-e-SAT a ser cancelado.';
    233: xMotivo := 'Rejeição: CPF do destinatário do CF-e-SAT de cancelamento diferente daquele do CF-e-SAT a ser cancelado.';
    234: xMotivo := 'Alerta: Razão Social/Nome do destinatário em branco';
    235: xMotivo := 'Rejeição: CNPJ do destinatario Invalido';
    236: xMotivo := 'Rejeição: Chave de Acesso com dígito verificador inválido';
    237: xMotivo := 'Rejeição: CPF do destinatario Invalido';
    238: xMotivo := 'Rejeição: CNPJ do emitente do CF-e-SAT de cancelamento diferente do CNPJ do CF-e-SAT a ser cancelado.';
    239: xMotivo := 'Rejeição: Versão do arquivo XML não suportada';
    240: xMotivo := 'Rejeição: Valor total do CF-e-SAT de cancelamento diferente do Valor total do CF-e-SAT a ser cancelado.';
    241: xMotivo := 'Rejeição: diferença de transmissão e recebimento da mensagem superior a 5 minutos.';
    242: xMotivo := 'Alerta: CFe dentro do lote estão fora de ordem.';
    243: xMotivo := 'Rejeição: XML Mal Formado';
    244: xMotivo := 'Rejeição: CNPJ do Certificado Digital difere do CNPJ da Matriz e do CNPJ do Emitente';
    245: xMotivo := 'Rejeição: CNPJ Emitente não autorizado para uso do SAT';
    246: xMotivo := 'Rejeição: Campo cUF inexistente no elemento cfeCabecMsg do SOAP Header';
    247: xMotivo := 'Rejeição: Sigla da UF do Emitente diverge da UF receptora';
    248: xMotivo := 'Rejeição: UF do Recibo diverge da UF autorizadora';
    249: xMotivo := 'Rejeição: UF da Chave de Acesso diverge da UF receptora';
    250: xMotivo := 'Rejeição: UF informada pelo SAT, não é atendida pelo Web Service';
    251: xMotivo := 'Rejeição: Certificado enviado não confere com o escolhido na declaração de posse';
    252: xMotivo := 'Rejeição: Ambiente informado diverge do Ambiente de recebimento';
    253: xMotivo := 'Rejeição: Digito Verificador da chave de acesso composta inválida';
    254: xMotivo := 'Rejeição: Elemento cfeCabecMsg inexistente no SOAP Header';
    255: xMotivo := 'Rejeição: CSR enviado inválido';
    256: xMotivo := 'Rejeição: CRT enviado inválido';
    257: xMotivo := 'Rejeição: Número do série do equipamento inválido';
    258: xMotivo := 'Rejeição: Data e/ou hora do envio inválida';
    259: xMotivo := 'Rejeição: Versão do leiaute inválida';
    260: xMotivo := 'Rejeição: UF inexistente';
    261: xMotivo := 'Rejeição: Assinatura digital não encontrada';
    262: xMotivo := 'Rejeição: CNPJ da software house não está ativo';
    263: xMotivo := 'Rejeição: CNPJ do contribuinte não está ativo';
    264: xMotivo := 'Rejeição: Base da receita federal está indisponível';
    265: xMotivo := 'Rejeição: Número de série inexistente no cadastro do equipamento';
    266: xMotivo := 'Falha na comunicação com a AC-SAT';
    267: xMotivo := 'Erro desconhecido na geração do certificado pela AC-SAT';
    268: xMotivo := 'Rejeição: Certificado está fora da data de validade.';
    269: xMotivo := 'Rejeição: Tipo de atividade inválida';
    270: xMotivo := 'Rejeição: Chave de acesso do CFe a ser cancelado inválido.';
    271: xMotivo := 'Rejeição: Ambiente informado no CF-e difere do Ambiente de recebimento cadastrado.';
    272: xMotivo := 'Rejeição: Valor do troco negativo.';
    273: xMotivo := 'Rejeição: Serviço Solicitado Inválido';
    274: xMotivo := 'Rejeição: Equipamento não possui declaração de posse';
    275: xMotivo := 'Rejeição: Status do equipamento diferente de Fabricado';
    276: xMotivo := 'Rejeição: Diferença de dias entre a data de emissão e de recepção maior que o prazo legal';
    277: xMotivo := 'Rejeição: CNPJ do emitente não está ativo junto à Sefaz na data de emissão';
    278: xMotivo := 'Rejeição: IE do emitente não está ativa junto à Sefaz na data de emissão';
    280: xMotivo := 'Rejeição: Certificado Transmissor Inválido'+sLineBreak+
                     ' - Certificado de Transmissor inexistente na Mensagem'+sLineBreak+
                     ' - Certificado de Transmissor inexistente na mensagem'+sLineBreak+
                     ' - Versão difere "3" - Se informado, Basic Constraint deve ser true (não pode ser Certificado de AC)'+sLineBreak+
                     ' - keyUsage não define "Autenticação Cliente"';
    281: xMotivo := 'Rejeição: Certificado Transmissor Data Validade (data de inicio e data fim)s';
    282: xMotivo := 'Rejeição: Certificado Transmissor sem CNPJ';
    283: xMotivo := 'Rejeição: Certificado Transmissor - erro Cadeia de Certificação';
    284: xMotivo := 'Rejeição: Certificado Transmissor revogado';
    285: xMotivo := 'Rejeição: Certificado Transmissor difere ICP-Brasil';
    286: xMotivo := 'Rejeição: Certificado Transmissor erro no acesso a LCR';
    287: xMotivo := 'Rejeição: Código Município do FG - ISSQN: dígito inválido. Exceto os códigos descritos no Anexo 2 que apresentam dígito inválido.';
    288: xMotivo := 'Rejeição: Data de emissão do CF-e-SAT a ser cancelado inválida';
    289: xMotivo := 'Rejeição: Código da UF informada diverge da UF solicitada';
    290: xMotivo := 'Rejeição: Certificado Assinatura inválido';
    291: xMotivo := 'Rejeição: Certificado Assinatura Data Validade';
    292: xMotivo := 'Rejeição: Certificado Assinatura sem CNPJ';
    293: xMotivo := 'Rejeição: Certificado Assinatura - erro Cadeia de Certificação';
    294: xMotivo := 'Rejeição: Certificado Assinatura revogado';
    295: xMotivo := 'Rejeição: Certificado Raiz difere dos Válidos';
    296: xMotivo := 'Rejeição: Certificado Assinatura erro no acesso a LCR';
    297: xMotivo := 'Rejeição: Assinatura difere do calculado';
    298: xMotivo := 'Rejeição: Assinatura difere do padrão do Projeto';
    299: xMotivo := 'Rejeição: Hora de emissão do CF-e-SAT a ser cancelado inválida';
    402: xMotivo := 'Rejeição: XML da área de dados com codificação diferente de UTF-8';
    403: xMotivo := 'Rejeição: Versão do leiaute do CF-e-SAT não é válida';
    404: xMotivo := 'Rejeição: Uso de prefixo de namespace não permitido';
    405: xMotivo := 'Alerta: Versão do leiaute do CF-e-SAT não é a mais atual';
    406: xMotivo := 'Rejeição: Versão do Software Básico do SAT não é valida.';
    407: xMotivo := 'Rejeição: Indicador de CF-e-SAT cancelamento inválido (diferente de „C? e „?)';
    408: xMotivo := 'Rejeição: Valor total do CF-e-SAT maior que o somatório dos valores de Meio de Pagamento empregados em seu pagamento.';
    409: xMotivo := 'Rejeição: Valor total do CF-e-SAT supera o máximo permitido no arquivo de Parametrização de Uso';
    410: xMotivo := 'Rejeição: UF informada no campo cUF não é atendida pelo Web Service';
    411: xMotivo := 'Rejeição: Campo versaoDados inexistente no elemento cfeCabecMsg do SOAP Header';
    412: xMotivo := 'Rejeição: CFe de cancelamento não corresponde ao CFe anteriormente gerado';
    420: xMotivo := 'Rejeição: Cancelamento para CF-e-SAT já cancelado';
    450: xMotivo := 'Rejeição: Modelo da CF-e-SAT diferente de 59';
    452: xMotivo := 'Rejeição: número de série do SAT inválido ou não autorizado.';
    453: xMotivo := 'Rejeição: Ambiente de processamento inválido (diferente de 1 e 2)';
    454: xMotivo := 'Rejeição: CNPJ da Software House inválido';
    455: xMotivo := 'Rejeição: Assinatura do Aplicativo Comercial não é válida.';
    456: xMotivo := 'Rejeição: Código de Regime tributário invalido';
    457: xMotivo := 'Rejeição: Código de Natureza da Operação para ISSQN inválido';
    458: xMotivo := 'Rejeição: Razão Social/Nome do destinatário em branco';
    459: xMotivo := 'Rejeição: Código do produto ou serviço em branco';
    460: xMotivo := 'Rejeição: GTIN do item (N) inválido';
    461: xMotivo := 'Rejeição: Descrição do produto ou serviço em branco';
    462: xMotivo := 'Rejeição: CFOP não é de operação de saída prevista para CF-e-SAT';
    463: xMotivo := 'Rejeição: Unidade comercial do produto ou serviço em branco';
    464: xMotivo := 'Rejeição: Quantidade Comercial do item (N) inválido';
    465: xMotivo := 'Rejeição: Valor unitário do item (N) inválido';
    466: xMotivo := 'Rejeição: Valor bruto do item (N) difere de quantidade * Valor Unitário, considerando regra de arred/trunc.';
    467: xMotivo := 'Rejeição: Regra de calculo do item (N) inválida';
    468: xMotivo := 'Rejeição: Valor do desconto do item (N) inválido';
    469: xMotivo := 'Rejeição: Valor de outras despesas acessórias do item (N) inválido.';
    470: xMotivo := 'Rejeição: Valor líquido do Item do CF-e difere de Valor Bruto de Produtos e Serviços - desconto + Outras Despesas Acessórias – rateio do desconto sobre subtotal + rateio do acréscimo sobre subtotal ';
    471: xMotivo := 'Rejeição: origem da mercadoria do item (N) inválido (difere de 0, 1, 2, 3, 4, 5, 6 e 7)';
    472: xMotivo := 'Rejeição: CST do Item (N) inválido (diferente de 00, 20, 90)';
    473: xMotivo := 'Rejeição: Alíquota efetiva do ICMS do item (N) inválido.';
    474: xMotivo := 'Rejeição: Valor líquido do ICMS do Item (N) difere de Valor do Item * Aliquota Efetiva';
    475: xMotivo := 'Rejeição: CST do Item (N) inválido (diferente de 40 e 41 e 50 e 60)';
    476: xMotivo := 'Rejeição: Código de situação da operação - Simples Nacional - do Item (N) inválido (diferente de 102, 300 e 500)';
    477: xMotivo := 'Rejeição: Código de situação da operação - Simples Nacional - do Item (N) inválido (diferente de 900)';
    478: xMotivo := 'Rejeição: Código de Situação Tributária do PIS Inválido (diferente de 01 e 02)';
    479: xMotivo := 'Rejeição: Base de cálculo do PIS do item (N) inválido.';
    480: xMotivo := 'Rejeição: Alíquota do PIS do item (N) inválido.';
    481: xMotivo := 'Rejeição: Valor do PIS do Item (N) difere de Base de Calculo * Aliquota do PIS';
    482: xMotivo := 'Rejeição: Código de Situação Tributária do PIS Inválido (diferente de 03)';
    483: xMotivo := 'Rejeição: Qtde Vendida do item (N) inválido.';
    484: xMotivo := 'Rejeição: Alíquota do PIS em R$ do item (N) inválido.';
    485: xMotivo := 'Rejeição: Valor do PIS do Item (N) difere de Qtde Vendida* Aliquota do PIS em R$';
    486: xMotivo := 'Rejeição: Código de Situação Tributária do PIS Inválido (diferente de 04, 06, 07, 08 e 09)';
    487: xMotivo := 'Rejeição: Código de Situação Tributária do PIS inválido (diferente de 49)';
    488: xMotivo := 'Rejeição: Código de Situação Tributária do PIS Inválido (diferente de 99)';
    489: xMotivo := 'Rejeição: Valor do PIS do Item (N) difere de Qtde Vendida* Aliquota do PIS em R$ e difere de Base de Calculo * Aliquota do PIS';
    490: xMotivo := 'Rejeição: Código de Situação Tributária da COFINS Inválido (diferente de 01 e 02)';
    491: xMotivo := 'Rejeição: Base de cálculo do COFINS do item (N) inválido.';
    492: xMotivo := 'Rejeição: Alíquota da COFINS do item (N) inválido.';
    493: xMotivo := 'Rejeição: Valor da COFINS do Item (N) difere de Base de Calculo * Aliquota da COFINS';
    494: xMotivo := 'Rejeição: Código de Situação Tributária da COFINS Inválido (diferente de 03)';
    495: xMotivo := 'Rejeição: Valor do COFINS do Item (N) difere de Qtde Vendida* Aliquota do COFINS em R$ e difere de Base de Calculo * Aliquota do COFINS';
    496: xMotivo := 'Rejeição: Alíquota da COFINS em R$ do item (N) inválido.';
    497: xMotivo := 'Rejeição: Valor da COFINS do Item (N) difere de Qtde Vendida* Aliquota da COFINS em R$';
    498: xMotivo := 'Rejeição: Código de Situação Tributária da COFINS Inválido (diferente de 04, 06, 07, 08 e 09)';
    499: xMotivo := 'Rejeição: Código de Situação Tributária da COFINS Inválido (diferente de 49)';
    500: xMotivo := 'Rejeição: Código de Situação Tributária da COFINS Inválido (diferente de 99)';
    501: xMotivo := 'Rejeição: Operação com tributação de ISSQN sem informar a Inscrição Municipal';
    502: xMotivo := 'Rejeição: Erro na Chave de Acesso - Campo Id não corresponde à concatenação dos campos correspondentes';
    503: xMotivo := 'Rejeição: Valor das deduções para o ISSQN do item (N) inválido.';
    504: xMotivo := 'Rejeição: Valor da Base de Calculo do ISSQN do Item (N) difere de Valor do Item - Valor das deduções';
    505: xMotivo := 'Rejeição: Alíquota efetiva do ISSQN do item (N) não é maior ou igual a 2,00 (2%) e menor ou igual a 5,00 (5%).';
    506: xMotivo := 'Valor do ISSQN do Item (N) difere de Valor da Base de Calculo do ISSQN * Alíquota Efetiva do ISSQN';
    507: xMotivo := 'Rejeição: Indicador de rateio para ISSQN inválido';
    508: xMotivo := 'Rejeição: Item da lista de Serviços do ISSQN do item (N) inválido.';
    509: xMotivo := 'Rejeição: Código municipal de Tributação do ISSQN do Item (N) em branco.';
    510: xMotivo := 'Rejeição: Código de Natureza da Operação para ISSQN inválido';
    511: xMotivo := 'Rejeição: Indicador de Incentivo Fiscal do ISSQN do item (N) inválido (diferente de 1 e 2)';
    512: xMotivo := 'Rejeição: Total do PIS difere do somatório do PIS dos itens';
    513: xMotivo := 'Rejeição: Total do COFINS difere do somatório do COFINS dos itens';
    514: xMotivo := 'Rejeição: Total do PIS-ST difere do somatório do PIS-ST dos itens';
    515: xMotivo := 'Rejeição: Total do COFINS-STdifere do somatório do COFINS-ST dos itens';
    516: xMotivo := 'Rejeição: Total de Outras Despesas Acessórias difere do somatório de Outras Despesas Acessórias (acréscimo) dos itens';
    517: xMotivo := 'Rejeição: Total dos Itens difere do somatório do valor líquido dos itens';
    518: xMotivo := 'Rejeição: Informado grupo de totais do ISSQN sem informar grupo de valores de ISSQN';
    519: xMotivo := 'Rejeição: Total da BC do ISSQN difere do somatório da BC do ISSQN dos itens';
    520: xMotivo := 'Rejeição: Total do ISSQN difere do somatório do ISSQN dos itens';
    521: xMotivo := 'Rejeição: Total do PIS sobre serviços difere do somatório do PIS dos itens de serviços';
    522: xMotivo := 'Rejeição: Total do COFINS sobre serviços difere do somatório do COFINS dos itens de serviços';
    523: xMotivo := 'Rejeição: Total do PIS-ST sobre serviços difere do somatório do PIS-ST dos itens de serviços';
    524: xMotivo := 'Rejeição: Total do COFINS-ST sobre serviços difere do somatório do COFINS-ST dos itens de serviços';
    525: xMotivo := 'Rejeição: Valor de Desconto sobre total inválido.';
    526: xMotivo := 'Rejeição: Valor de Acréscimo sobre total inválido.';
    527: xMotivo := 'Rejeição: Código do Meio de Pagamento inválido';
    528: xMotivo := 'Rejeição: Valor do Meio de Pagamento inválido.';
    529: xMotivo := 'Rejeição: Valor de desconto sobre subtotal difere do somatório dos seus rateios nos itens.';
    530: xMotivo := 'Rejeição: Operação com tributação de ISSQN sem informar a Inscrição Municipal';
    531: xMotivo := 'Rejeição: Valor de acréscimo sobre subtotal difere do somatório dos seus rateios nos itens.';
    532: xMotivo := 'Rejeição: Total do ICMS difere do somatório dos itens';
    533: xMotivo := 'Rejeição: Valor aproximado dos tributos do CF-e-SAT – Lei 12741/12 inválido';
    534: xMotivo := 'Rejeição: Valor aproximado dos tributos do Produto ou serviço – Lei 12741/12 inválido.';
    535: xMotivo := 'Rejeição: código da credenciadora de cartão de débito ou crédito inválido';
    536: xMotivo := 'Rejeição: código da credenciadora de cartão de débito ou crédito não informado para meio de pagamento cartão de débito ou crédito. (Ver exceções no Anexo 06)';
    537: xMotivo := 'Rejeição: Total do Desconto difere do somatório dos itens';
    539: xMotivo := 'Rejeição: Duplicidade de CF-e-SAT, com diferença na Chave de Acesso [99999999999999999999999999999999999999999]';
    540: xMotivo := 'Rejeição: CNPJ da Software House + CNPJ do emitente assinado no campo “signAC” difere do informado no campo “CNPJvalue” ';
    555: xMotivo := 'Rejeição: Tipo autorizador do protocolo diverge do Órgão Autorizador';
    564: xMotivo := 'Rejeição: Total dos Produtos ou Serviços difere do somatório do valor dos Produtos ou Serviços dos itens';
    600: xMotivo := 'Serviço Temporariamente Indisponível';
    601: xMotivo := 'CF-e-SAT inidôneo por recepção fora do prazo';
    602: xMotivo := 'Rejeição: Status do equipamento não permite ativação';
    603: xMotivo := 'Arquivo inválido';
    604: xMotivo := 'Erro desconhecido na verificação de comandos';
    605: xMotivo := 'Tamanho do arquivo inválido';
    612: xMotivo := 'Rejeição: NCM não Informado';
    613: xMotivo := 'Rejeição: NCM inválido, fora do range especificado';
    614: xMotivo := 'Rejeição: NCM 00 não aceito para o GTIN informado';
    999: xMotivo := 'Rejeição: Erro não catalogado';
  else
    xMotivo := 'Rejeição não catalogada na nota técnica 2013/001.';
  end;

  Result := ACBrStr(xMotivo);
end;

function MotivoInvalidoVenda(cod: integer): String;
begin
  case cod of
    1002 : Result := 'Código da UF não confere com a Tabela do IBGE'; // | Válido até 31/12/2015
    1003 : Result := 'Código da UF diferente da UF registrada no SAT';// | Válido até 31/12/2015
    1004 : Result := 'Versão do leiaute do arquivo de entrada do SAT não é válida';
    1005 : Result := 'Alerta Versão do leiaute do arquivo de entrada do SAT não é a mais atual';
    1226 : Result := 'Código da UF do Emitente diverge da UF receptora';
    1450 : Result := 'Código de modelo de documento fiscal diferente de 59';
    1258 : Result := 'Data/hora inválida. Problemas com o relógio interno do SAT-CF-e';
    1224 : Result := 'CNPJ da Software House inválido';
    1455, 1222 : Result := 'Assinatura do Aplicativo Comercial não é válida';// | Válido até 31/12/2015
    1207 : Result := 'CNPJ do emitente inválido';
    1203 : Result := 'Emitente não autorizado para uso do SAT';
    1229 : Result := 'IE do emitente não informada C12 IE não corresponde ao Contribuinte de uso do SAT';
    1230 : Result := 'IE do emitente diferente da IE do contribuinte autorizado para uso do SAT';// | Checar com dado recebido na parametrização do SAT
    1457 : Result := 'Código de Natureza da Operação para ISSQN inválido';
    1507 : Result := 'Indicador de rateio para ISSQN inválido';
    1235 : Result := 'CNPJ do destinatário inválido';
    1237 : Result := 'CPF do destinatário inválido';
    1234 : Result := 'Alerta Razão Social/Nome do destinatário em branco';//| Válido até 31/12/2015
    1019 : Result := 'Numeração dos itens não é sequencial crescente';
    1459 : Result := 'Código do produto ou serviço em branco';
    1460 : Result := 'GTIN do item (N) inválido | Validação do dígito verificador';
    1461 : Result := 'Descrição do produto ou serviço em branco';
    1470 : Result := 'NCM não informado';
    1471 : Result := 'Origem da mercadoria do Item (N) inválido (diferente de 0, 1, 2, 3, 4, 5, 6, 7, 8)';
    1472 : Result := 'CST do Item (N) inválido (diferente de 00, 01, 12, 13, 14, 20, 21, 72, 73, 74, 90)';
    1462 : Result := 'CFOP do item (N) inválido (Código informado não consta na tabelaCFOP)';
    1463 : Result := 'Unidade Comercial do produto ou serviço em branco';
    1464 : Result := 'Quantidade Comercial do item (N) inválido';
    1465 : Result := 'Valor Unitário do item (N) inválido';
    1467 : Result := 'Regra de cálculo do Item (N) inválido (diferente de "A" e "T")';
    1468 : Result := 'Valor do Desconto do item (N) inválido';
    1469 : Result := 'Valor de outras despesas acessórias do item (N) inválido';
    1535 : Result := 'Código da credenciadora de cartão de débito ou crédito inválido';
    1536 : Result := 'Código da credenciadora de cartão de débito ou crédito não informado para meio de pagamento cartão de débito ou crédito';
    1537 : Result := 'Alerta: Código de Autenticação de Pagamento de cartão de débito ou crédito não informado para meio de pagamento cartão de débito ou crédito';
    1220 : Result := 'Valor do rateio do desconto sobre subtotal do item (N) inválido';
    1228 : Result := 'Valor do rateio do acréscimo sobre subtotal do item (N) inválido';
    1751 : Result := 'não informado código do produto';
    1752 : Result := 'código de produto informado fora do padrão ANP';
    1534 : Result := 'Valor aproximado dos tributos do produto negativo';
    1533 : Result := 'Valor aproximado dos tributos do CF-e_SAT negativo';
    1473 : Result := 'Alíquota efetiva do ICMS do item (N) não é maior ou igual a zero';
    1601 : Result := 'Alerta Código de regime tributário é incompatível com o grupo de ICMS00';
    1475 : Result := 'CST do Item (N) inválido (diferente de 30, 40, 41, 60, 61)';
    1602 : Result := 'Alerta Código de regime tributário é incompatível com o grupo de ICMS40';
    1476 : Result := 'Código de situação da operação - Simples Nacional - do Item (N) inválido (diferente de 102, 300 e 500)';
    1603 : Result := 'Alerta Código de regime tributário é incompatível com o grupo de ICMSSN102';
    1477 : Result := 'Código de situação da operação - Simples Nacional - do Item (N) inválido (diferente de 900)';
    1604 : Result := 'Alerta Código de regime tributário é incompatível com o grupo de ICMSSN900';
    1478 : Result := 'Código de Situação Tributária do PIS Inválido (diferente de 01, 02 e 05)';
    1479 : Result := 'Base de cálculo do PIS do item (N) inválido';
    1480 : Result := 'Alíquota do PIS do item (N) não é maior ou igual a zero';
    1482 : Result := 'Código de Situação Tributária do PIS Inválido (diferente de 03)';
    1483 : Result := 'Qtde Vendida do item (N) não é maior ou igual a zero';
    1484 : Result := 'Alíquota do PIS em R$ do item (N) não é maior ou igual a zero';
    1486 : Result := 'Código de Situação Tributária do PIS Inválido (diferente de 04, 06, 07, 08 e 09)';
    1487 : Result := 'Código de Situação Tributária do PIS inválido (diferente de 49)';
    1488 : Result := 'Código de Situação Tributária do PIS Inválido (diferente de 99)';
    1490 : Result := 'Código de Situação Tributária da COFINS Inválido (diferente de 01, 02 e 05)';
    1491 : Result := 'Base de cálculo do COFINS do item (N) inválido';
    1492 : Result := 'Alíquota da COFINS do item (N) não é maior ou igual a zero';
    1494 : Result := 'Código de Situação Tributária da COFINS Inválido (diferente de 03)';
    1496 : Result := 'Alíquota da COFINS em R$ do item (N) não é maior ou igual a zero';
    1498 : Result := 'Código de Situação Tributária da COFINS Inválido (diferente de 04, 06, 07, 08 e 09)';
    1499 : Result := 'Código de Situação Tributária da COFINS Inválido (diferente de 49)';
    1500 : Result := 'Código de Situação Tributária da COFINS Inválido (diferente de 99)';
    1501 : Result := 'Operação com tributação de ISSQN sem informar a Inscrição Municipal';
    1503 : Result := 'Valor das deduções para o ISSQN do item (N) não é maior ou igual a zero';
    1505 : Result := 'Alíquota efetiva do ISSQN do item (N) não é maior ou igual a 2,00 (2%) e menor ou igual a 5,00 (5%)';
    1287 : Result := 'Código Município do FG - ISSQN: dígito inválido. Exceto os códigos descritos no Anexo 2 que apresentam dígito inválido';
    1509 : Result := 'Código municipal de Tributação do ISSQN do Item (N) em branco';
    1510 : Result := 'Código de Natureza da Operação para ISSQN inválido';
    1511 : Result := 'Indicador de Incentivo Fiscal do ISSQN do item (N) inválido (diferente de 1 e 2)';
    1527 : Result := 'Código do Meio de Pagamento inválido';
    1528 : Result := 'Valor do Meio de Pagamento inválido';
    1408 : Result := 'Valor total do CF-e-SAT maior que o somatório dos valores de Meio de Pagamento empregados em seu pagamento';
    1409 : Result := 'Valor total do CF-e-SAT supera o máximo permitido no arquivo de Parametrização de Uso';
    1073 : Result := 'Valor de Desconto sobre total não é maior ou igual a zero';
    1074 : Result := 'Valor de Acréscimo sobre total não é maior ou igual a zero';
    1084 : Result := 'Erro Formatação do Certificado não é válido';
    1085 : Result := 'Erro Assinatura do Aplicativo Comercial não confere com o registro do SAT'; //| Válido até 31/12/2015
    1998 : Result := 'Não é possível gerar o cupom com os dados de entrada informados, pois resultam valores negativos';
    1999 : Result := 'Erro não identificado';
  else
    Result := 'Erro não identificado';
  end;

  Result := ACBrStr(Result);
end;

function MotivoInvalidoCancelamento(cod: integer): String;
begin
  case cod of
    1270 : Result := 'Chave de acesso do CFe a ser cancelado inválido';
    1412 : Result := 'CFe de cancelamento não corresponde a um CFe emitido nos 30 minutos anteriores ao pedido de cancelamento';
    1258 : Result := 'Data/hora inválida. Problemas com o relógio interno do SAT-CF-e';
    1210 : Result := 'Intervalo de tempo entre a emissão do CF-e a ser cancelado e a emissão do respectivo CF-e de cancelamento é maior que 30 (trinta) minutos';
    1454 : Result := 'CNPJ da Software House inválido';
    1455 : Result := 'Assinatura do Aplicativo Comercial não é válida';
    1232 : Result := 'CNPJ do destinatário do CF-e de cancelamento diferente daquele do CF-e a ser cancelado';
    1233 : Result := 'CPF do destinatário do CF-e de cancelamento diferente daquele do CF-e a ser cancelado';
    1218 : Result := 'Erro Chave de acesso do CF-e-SAT já consta como cancelado'; //Nova redação a partir de 01.01.16
    1999 : Result := 'Erro não identificado'; //Implementação facultativa até 31.12.15 e obrigatória a partir de 01.01.16.
  else
    Result := 'Erro não identificado';
  end;

  Result := ACBrStr(Result);
end;

{ TACBrSAT }

constructor TACBrSAT.Create(AOwner : TComponent) ;
begin
  inherited Create(AOwner) ;

  fsnumeroSessao    := 0;
  fsNomeDLL         := '';
  fsArqLOG          := '' ;
  fsComandoLog      := '';
  fsRespostaComando := '';
  fsAguardandoResposta := False;

  fsValidarNumeroSessaoResposta := False;
  fsNumeroTentativasValidarSessao := CMAX_ERROS_SESSAO;
  fsErrosSessaoCount := 0;
  fsSessaoAVerificar := 0;

  fsOnGetcodigoDeAtivacao         := Nil;
  fsOnGetsignAC                   := Nil;
  fsOnGravarLog                   := Nil;
  fsOnGetNumeroSessao             := Nil;
  fsOnAguardandoRespostaChange    := Nil;
  fsOnCancelarUltimaVenda         := Nil;
  fsOnConsultarNumeroSessao       := Nil;
  fsOnConsultarSAT                := Nil;
  fsOnConsultaStatusOperacional   := Nil;
  fsOnEnviarDadosVenda            := Nil;
  fsOnExtrairLogs                 := Nil;
  fsOnMensagemSEFAZ               := Nil;
  fsOnConsultarUltimaSessaoFiscal := Nil;

  fsConfig := TACBrSATConfig.Create(Self);
  fsConfig.Name := 'ACBrSATConfig' ;
  {$IFDEF COMPILER6_UP}
  fsConfig.SetSubComponent( true );{ para gravar no DFM/XFM }
  {$ENDIF}

  fsConfigArquivos := TACBrSATConfigArquivos.Create(Self);
  fsConfigArquivos.Name := 'ACBrSATConfigArquivos' ;
  {$IFDEF COMPILER6_UP}
  fsConfigArquivos.SetSubComponent( true );{ para gravar no DFM/XFM }
  {$ENDIF}

  fsSSL     := TDFeSSL.Create;
  fsRede    := TRede.Create;
  fsCFe     := TCFe.Create;
  fsCFeCanc := TCFeCanc.Create;
  fsResposta:= TACBrSATResposta.Create;
  fsStatus  := TACBrSATStatus.Create;
  fsSATClass:= TACBrSATClass.Create( Self ) ;
  // Italo
  fsLoteCFe := TLoteCFeCollection.Create;
  fsLoteCFeCanc := TLoteCFeCancCollection.Create;

  fsPrefixoCFe := CPREFIXO_CFe;
end ;

destructor TACBrSAT.Destroy ;
begin
  fsConfig.Free;
  fsConfigArquivos.Free;
  fsRede.Free;
  fsCFe.Free;
  fsCFeCanc.Free;
  fsResposta.Free;
  fsStatus.Free;
  fsSSL.Free;
  // Italo
  fsLoteCFe.Free;
  fsLoteCFeCanc.Free;

  if Assigned( fsSATClass ) then
    FreeAndNil( fsSATClass );

  inherited Destroy ;
end ;

procedure TACBrSAT.Inicializar ;
begin
  if fsInicializado then exit ;

  if fsModelo = satNenhum then
     raise EACBrSATErro.Create( cACBrSATModeloNaoDefinido );

  fsSATClass.Inicializar ;
  Randomize;

  DoLog( 'ACBrSAT.Inicializado');

  fsInicializado := true ;
  fsAguardandoResposta := False;
  fsPrefixoCFe := CPREFIXO_CFe;

  if (fsConfig.infCFe_versaoDadosEnt <= 0) then
    fsConfig.infCFe_versaoDadosEnt := VerificarVersaoSAT;
end ;

function TACBrSAT.LerLoteCFe(const AFileName: String;
  AGravarXml: Boolean = True): boolean;
var
  SL: TStringList;
  aLote, aXml, NomeArq: string;
  i, f, n, j: Integer;
  CFeCanc: Boolean;

  function PosCFe: Integer;
  begin
    if CFeCanc then
      Result := Pos('</CFeCanc>', aLote)
    else
      Result := Pos('</CFe>', aLote);
  end;

begin
  Result := False;
  CFeCanc := False;

  fsLoteCFe.Clear;
  fsLoteCFeCanc.Clear;
  
  SL := TStringList.Create;
  try
    SL.LoadFromFile( AFileName );
    aLote := SL.Text;
  finally
    SL.Free;
  end;

  i := Pos('<LoteCFe>', aLote);
  f := Pos('</LoteCFe>', aLote);

  if (i = 0) and (f = 0) then
  begin
    i := Pos('<LoteCFeCanc>', aLote);
    f := Pos('</LoteCFeCanc>', aLote);
    CFeCanc := (i > 0);
  end;

  if i > 0 then
  begin
    aLote := Copy(aLote, 1, f - 1);

    if CFeCanc then
      aLote := Copy(aLote, i + 13, Length(aLote))
    else
      aLote := Copy(aLote, i + 9, Length(aLote));

    n := PosCFe;

    while n > 0 do
    begin
      if CFeCanc then
      begin
        aXml := copy(aLote, 1, n + 9);
        aLote := Trim(copy(aLote, N + 10, length(aLote)));

        // Popula a classe fsLoteCFe com os CF-e Cancelados lidos
        fsLoteCFeCanc.New.SetXMLString(aXML);
      end
      else
      begin
        aXml := copy(aLote, 1, n + 5);
        aLote := Trim(copy(aLote, N + 6, length(aLote)));

        // Popula a classe fsLoteCFe com os CF-e lidos
        fsLoteCFe.New.SetXMLString(aXML);
      end;

      // Salvar em disco
      if AGravarXml then
      begin
        if CFeCanc then
        begin
          j := fsLoteCFeCanc.Count - 1;

          fsCFeCanc.Clear;

          fsCFeCanc.Emit.CNPJ := fsLoteCFeCanc[j].Emit.CNPJ;
          fsCFeCanc.ide.dEmi := fsLoteCFeCanc[j].ide.dEmi;
          fsCFeCanc.infCFe.ID := fsLoteCFeCanc[j].infCFe.ID;
          fsCFeCanc.infCFe.chCanc := fsLoteCFeCanc[j].infCFe.chCanc;

          NomeArq := CalcCFeCancNomeArq(fsConfigArquivos.PastaCFeCancelamento);
        end
        else
        begin
          j := fsLoteCFe.Count - 1;

          fsCFe.Clear;

          fsCFe.Emit.CNPJ := fsLoteCFe[j].Emit.CNPJ;
          fsCFe.ide.dEmi := fsLoteCFe[j].ide.dEmi;
          fsCFe.infCFe.ID := fsLoteCFe[j].infCFe.ID;

          NomeArq := CalcCFeNomeArq(fsConfigArquivos.PastaCFeVenda);
        end;

        WriteToTXT(NomeArq, aXml, False, False, True);
      end;

      n := PosCFe;
    end;

    Result := True;
  end;
end;

procedure TACBrSAT.DesInicializar ;
begin
  if not fsInicializado then exit ;

  fsSATClass.DesInicializar ;
  DoLog( 'ACBrSAT.DesInicializado');

  fsInicializado := false;
  fsStatus.Clear;
  fsResposta.Clear;
  fsRede.Clear;
end ;

procedure TACBrSAT.VerificaInicializado ;
begin
  if not Inicializado then
     raise EACBrSATErro.Create( cACBrSATNaoInicializado ) ;
end ;

procedure TACBrSAT.IniciaComando ;
var
  AStr : String ;
begin
  VerificaInicializado;
  if fsAguardandoResposta then
     raise EACBrSATErro.CreateFmt( cACBrSATOcupadoException, [numeroSessao] ) ;

  fsSessaoAVerificar := 0;
  GerarnumeroSessao;

  fsRespostaComando := '';
  AStr := 'NumeroSessao: '+IntToStr(numeroSessao) ;
  if fsComandoLog <> '' then
     AStr := AStr + ' - Comando: '+fsComandoLog;

  DoLog( AStr );
  SetAguardandoResposta(True);
end ;

function TACBrSAT.FinalizaComando( const AResult : String ) : String ;
var
  AStr : String ;
  SessaoEnviada: Integer;
begin
  fsRespostaComando := DecodificarPaginaDeCodigoSAT( AResult );
  Result := fsRespostaComando;

  SetAguardandoResposta(False);

  fsComandoLog := '';
  AStr := 'NumeroSessao: '+IntToStr(numeroSessao) ;
  if fsRespostaComando <> '' then
     AStr := AStr + ' - Resposta:'+fsRespostaComando;

  DoLog( AStr );
  Resposta.RetornoStr := fsRespostaComando;

  if (Resposta.numeroSessao <> numeroSessao) then
  begin
    if (fsSessaoAVerificar >= 0) and (Resposta.numeroSessao <> fsSessaoAVerificar) then
    begin
      if fsSessaoAVerificar = 0 then
        SessaoEnviada := numeroSessao
      else
        SessaoEnviada := fsSessaoAVerificar;

      AStr := Format('ERRO: Sessao retornada pelo SAT [%d], diferente da enviada [%d].',
                     [Resposta.numeroSessao, numeroSessao] );
      DoLog( '   '+AStr);

      if fsValidarNumeroSessaoResposta then    // Tenta se recuperar da resposta inválida ?
      begin
        Inc( fsErrosSessaoCount );
        if fsErrosSessaoCount > fsNumeroTentativasValidarSessao then
          raise EACBrSATErro.Create(AStr);

        AStr := Format('   Consultando Sessao [%d], tentativa: %d', [SessaoEnviada, fsErrosSessaoCount]);
        DoLog(AStr);
        ConsultarNumeroSessao(SessaoEnviada);
        Exit;
      end;
    end
    else
    begin
      if fsSessaoAVerificar > 0 then
        DoLog(Format('   Sessao [%d] recuperada com sucesso',[fsSessaoAVerificar]));
    end;
  end
  else
  begin
    if (Resposta.codigoDeRetorno = 11003) and  // 11003 = Sessão não existe
       (fsSessaoAVerificar > 0) and
       (fsErrosSessaoCount > 0) then
    begin
      raise EACBrSATErro.Create(Format('ERRO: SAT nao respondeu a sessao [%d]', [fsSessaoAVerificar] ));
    end;
  end;

  fsErrosSessaoCount := 0;
  if Assigned(fsOnMensagemSEFAZ) then
    if (Resposta.codigoSEFAZ > 0) or (Resposta.mensagemSEFAZ <> '') then
      fsOnMensagemSEFAZ( Resposta.codigoSEFAZ, Resposta.mensagemSEFAZ );
end ;

procedure TACBrSAT.VerificaCondicoesImpressao(EhCancelamento: Boolean);
begin
  if not Assigned(Extrato) then
    raise EACBrSATErro.Create( 'Nenhum componente "ACBrSATExtrato" associado' ) ;

  if EhCancelamento then
  begin
    if (CFeCanc.infCFe.ID = '') and (CFe.infCFe.ID = '') then
      raise EACBrSATErro.Create( 'Nenhum CFeCanc ou CFe carregado na memória' ) ;
  end
  else
  begin
    if (CFe.infCFe.ID = '') then
      raise EACBrSATErro.Create( 'Nenhum CFe carregado na memória' ) ;
  end;
end;

procedure TACBrSAT.DoLog(const AString : String) ;
var
  Tratado: Boolean;
begin
  Tratado := False;
  if Assigned( fsOnGravarLog ) then
    fsOnGravarLog( AString, Tratado );

  if not Tratado then
    GravaLog( AString );
end ;

procedure TACBrSAT.GravaLog(const AString : AnsiString) ;
begin
  if (ArqLOG = '') then
    Exit;

  WriteLog( ArqLOG, FormatDateTime('dd/mm/yy hh:nn:ss:zzz',now) + ' - ' + AString );
end ;

function TACBrSAT.GerarnumeroSessao : Integer ;
begin
  fsnumeroSessao := Random(999999);

  if Assigned( fsOnGetNumeroSessao ) then
     fsOnGetNumeroSessao( fsnumeroSessao ) ;

  Result := fsnumeroSessao;
end ;

procedure TACBrSAT.InicializaCFe(ACFe : TCFe) ;
Var
  wCFe : TCFe ;
begin
  if Assigned( ACFe ) then
    wCFe := ACFe
  else
    wCFe := fsCFe;

  with wCFe do
  begin
    Clear;
    ide.CNPJ              := fsConfig.ide_CNPJ;
    ide.tpAmb             := fsConfig.ide_tpAmb;
    ide.numeroCaixa       := fsConfig.ide_numeroCaixa;
    ide.signAC            := signAC;
    ide.modelo            := 59;
    Emit.CNPJ             := fsConfig.emit_CNPJ;
    Emit.IE               := fsConfig.emit_IE;
    Emit.IM               := fsConfig.emit_IM;
    Emit.cRegTrib         := fsConfig.emit_cRegTrib;
    Emit.cRegTribISSQN    := fsConfig.emit_cRegTribISSQN;
    Emit.indRatISSQN      := fsConfig.emit_indRatISSQN;
    infCFe.versaoDadosEnt := fsConfig.infCFe_versaoDadosEnt;
  end ;
end ;

function TACBrSAT.AssociarAssinatura(CNPJvalue: AnsiString; const assinaturaCNPJs : AnsiString) : String ;
var
  SATResp: String;
begin
  CNPJvalue := OnlyNumber(CNPJvalue);
  fsComandoLog := 'AssociarAssinatura( '+CNPJvalue+', '+assinaturaCNPJs+' )';
  IniciaComando;
  try
    SATResp := fsSATClass.AssociarAssinatura( CNPJvalue, assinaturaCNPJs );
  finally
    Result := FinalizaComando( SATResp );
  end;
end ;

function TACBrSAT.AtivarSAT(subComando : Integer ; CNPJ : AnsiString ;
  cUF : Integer) : String ;
var
  SATResp: String;
begin
  CNPJ := OnlyNumber(CNPJ);
  fsComandoLog := 'AtivarSAT( '+IntToStr(subComando)+', '+CNPJ+', '+IntToStr(cUF)+' )';
  IniciaComando;
  try
    SATResp := fsSATClass.AtivarSAT( subComando, CNPJ, cUF )
  finally
    Result := FinalizaComando( SATResp );
  end;
end ;

function TACBrSAT.AtualizarSoftwareSAT : String ;
var
  SATResp: String;
begin
  fsComandoLog := 'AtualizarSoftwareSAT';
  IniciaComando;
  try
    SATResp := fsSATClass.AtualizarSoftwareSAT;
  finally
    Result := FinalizaComando( SATResp );
  end;
end ;

function TACBrSAT.BloquearSAT : String ;
var
  SATResp: String;
begin
  fsComandoLog := 'BloquearSAT';
  IniciaComando;
  try
    SATResp := fsSATClass.BloquearSAT;
  finally
    Result := FinalizaComando( SATResp );
  end;
end ;

function TACBrSAT.CancelarUltimaVenda: String ;
var
  dadosCancelamento : string;
begin
  CFe2CFeCanc; // Atualiza para chave carregada para o cancelamento

  dadosCancelamento := CFeCanc.GerarXML( true ); // True = Gera apenas as TAGs da aplicação

  Result := CancelarUltimaVenda( CFeCanc.infCFe.chCanc, dadosCancelamento);
end ;


function TACBrSAT.CancelarUltimaVenda(chave, dadosCancelamento : AnsiString
  ) : String ;
var
  NomeCFe: String;
  SATResp: String;
begin
  fsComandoLog := 'CancelarUltimaVenda( '+chave+', '+dadosCancelamento+' )';

  chave := Trim(chave);
  if chave = '' then
     raise EACBrSATErro.Create('Parâmetro: "chave" não informado');

  dadosCancelamento := Trim(dadosCancelamento);
  if dadosCancelamento = '' then
     raise EACBrSATErro.Create('Parâmetro: "dadosCancelamento" não informado');

  // Dados deve sempre ser enviados em UTF8
  dadosCancelamento := ConverteXMLtoUTF8(dadosCancelamento);

  if fsConfigArquivos.SalvarEnvio then
  begin
    NomeCFe := CalcCFeCancNomeArq(fsConfigArquivos.PastaEnvio, '', '-env');
    WriteToTXT(NomeCFe, dadosCancelamento, False, False);
    DoLog('  Gravando XML Cancelamento enviado: '+NomeCFe);
  end;

  IniciaComando;
  try
    SATResp := '';
    if Assigned(fsOnCancelarUltimaVenda) then
      fsOnCancelarUltimaVenda(dadosCancelamento, SATResp);

    if EstaVazio(SATResp) then
      SATResp := fsSATClass.CancelarUltimaVenda(chave, dadosCancelamento);
  finally
    Result := FinalizaComando( SATResp ) ;
  end;

  DecodificaRetorno7000;
end ;

function TACBrSAT.ComunicarCertificadoICPBRASIL(const certificado : AnsiString) : String ;
var
  SATResp: String;
begin
  fsComandoLog := 'ComunicarCertificadoICPBRASIL( '+certificado+' )';
  IniciaComando;
  try
    SATResp := fsSATClass.ComunicarCertificadoICPBRASIL( certificado );
  finally
    Result := FinalizaComando( SATResp );
  end;
end ;

function TACBrSAT.ConfigurarInterfaceDeRede(dadosConfiguracao : AnsiString ) : String ;
var
  SATResp: String;
begin
  if dadosConfiguracao = '' then
    dadosConfiguracao := Rede.AsXMLString
  else
    Rede.AsXMLString := dadosConfiguracao;

  fsComandoLog := 'ConfigurarInterfaceDeRede( '+dadosConfiguracao+' )';
  IniciaComando;
  try
    SATResp := fsSATClass.ConfigurarInterfaceDeRede( dadosConfiguracao );
  finally
    Result := FinalizaComando( SATResp );
  end;
end ;

function TACBrSAT.ConsultarNumeroSessao(cNumeroDeSessao : Integer ) : String ;
var
  SATResp: String;
begin
  fsComandoLog := 'ConsultarNumeroSessao( '+IntToStr(cNumeroDeSessao)+' )';
  IniciaComando;
  try
    fsSessaoAVerificar := cNumeroDeSessao;

    SATResp := '';
    if Assigned(fsOnConsultarNumeroSessao) then
      fsOnConsultarNumeroSessao(IntToStr(cNumeroDeSessao), SATResp);

    if EstaVazio(SATResp) then
      SATResp := fsSATClass.ConsultarNumeroSessao( cNumeroDeSessao );
  finally
    Result := FinalizaComando( SATResp );
  end;

  DecodificaRetorno6000;
  DecodificaRetorno7000;
end ;

function TACBrSAT.ConsultarUltimaSessaoFiscal: String;
var
  SATResp: String;
begin
  fsComandoLog := 'ConsultarUltimaSessaoFiscal';
  IniciaComando;
  try
    fsSessaoAVerificar := -1;  // Não sabemos qual será o Número de sessão retornado...

    SATResp := '';
    if Assigned(fsOnConsultarUltimaSessaoFiscal) then
      fsOnConsultarUltimaSessaoFiscal(SATResp);

    if EstaVazio(SATResp) then
      SATResp := fsSATClass.ConsultarUltimaSessaoFiscal;
  finally
    Result := FinalizaComando( SATResp );
  end;

  DecodificaRetorno6000;
  DecodificaRetorno7000;
end;

function TACBrSAT.ConsultarSAT : String ;
var
  SATResp: String;
begin
  fsComandoLog := 'ConsultarSAT';
  IniciaComando;
  try
    SATResp := '';
    if Assigned(fsOnConsultarSAT) then
      fsOnConsultarSAT(SATResp);

    if EstaVazio(SATResp) then
      SATResp := fsSATClass.ConsultarSAT;
  finally
    Result := FinalizaComando( SATResp );
  end;
end ;

function TACBrSAT.ConsultarStatusOperacional : String ;
Var
  ok: Boolean;
  I: Integer;
  AStr, SATResp: String;
begin
  fsComandoLog := 'ConsultarStatusOperacional';
  IniciaComando;
  try
    SATResp := '';
    if Assigned(fsOnConsultaStatusOperacional) then
      fsOnConsultaStatusOperacional(SATResp);

    if EstaVazio(SATResp) then
      SATResp := fsSATClass.ConsultarStatusOperacional;
  finally
    Result := FinalizaComando( SATResp ) ;
  end;

  ok := True;

  if fsResposta.codigoDeRetorno = 10000 then
  begin
    with fsRede do
    begin
      tipoLan := StrToTipoLan(ok, fsResposta.RetornoLst[06]) ;
      lanIP   := fsResposta.RetornoLst[07];
      lanMask := fsResposta.RetornoLst[09];
      lanGW   := fsResposta.RetornoLst[10];
      lanDNS1 := fsResposta.RetornoLst[11];
      lanDNS2 := fsResposta.RetornoLst[12];
    end;

    with fsStatus do
    begin
      Clear;
      NSERIE         := fsResposta.RetornoLst[05];
      LAN_MAC        := fsResposta.RetornoLst[08];
      STATUS_LAN     := StrToStatusLan(ok, fsResposta.RetornoLst[13]) ;;
      NIVEL_BATERIA  := StrToNivelBateria(ok, fsResposta.RetornoLst[14]) ;;
      MT_TOTAL       := fsResposta.RetornoLst[15];
      MT_USADA       := fsResposta.RetornoLst[16];
      DH_ATUAL       := StoD( fsResposta.RetornoLst[17] );
      VER_SB         := fsResposta.RetornoLst[18];
      VER_LAYOUT     := fsResposta.RetornoLst[19];
      ULTIMO_CFe     := fsResposta.RetornoLst[20];
      LISTA_INICIAL  := fsResposta.RetornoLst[21];

      { Workaround para leitura de Status do Emulador do Fiscl,
        que não retorna o campo: LISTA_FINAL }
      I := 22;
      if fsResposta.RetornoLst.Count > 27 then
      begin
        LISTA_FINAL  := fsResposta.RetornoLst[22];
        Inc(I);
      end;
      DH_CFe         := StoD( fsResposta.RetornoLst[I] );
      Inc(I);
      DH_ULTIMA      := StoD( fsResposta.RetornoLst[I] );
      Inc(I);
      CERT_EMISSAO   := StoD( fsResposta.RetornoLst[I] ) ;
      Inc(I);
      CERT_VENCIMENTO:= StoD( fsResposta.RetornoLst[I] ) ;
      Inc(I);
      AStr := fsResposta.RetornoLst[I];
      if StrIsNumber(AStr) then
        ESTADO_OPERACAO := TACBrSATEstadoOperacao( StrToInt(AStr) )
      else
        ESTADO_OPERACAO:= StrToEstadoOperacao(ok, AStr) ;;
    end;
  end;
end ;

function TACBrSAT.DesbloquearSAT : String ;
var
  SATResp: String;
begin
  fsComandoLog := 'DesbloquearSAT';
  IniciaComando;
  try
    SATResp := fsSATClass.DesbloquearSAT;
  finally
    Result := FinalizaComando( SATResp );
  end;
end ;

function TACBrSAT.EnviarDadosVenda: String;
begin
  Result := EnviarDadosVenda( CFe.GerarXML( True ) );  // True = Gera apenas as TAGs da aplicação
end;

function TACBrSAT.EnviarDadosVenda(dadosVenda : AnsiString) : String ;
var
  NomeCFe, SATResp: String;
  tini, tfim: TDateTime;
begin
  dadosVenda := Trim(dadosVenda);

  fsComandoLog := 'EnviarDadosVenda( '+dadosVenda+' )';
  if dadosVenda = '' then
     raise EACBrSATErro.Create('Parâmetro: "dadosVenda" não informado');

  // Dados deve sempre ser enviados em UTF8
  dadosVenda := ConverteXMLtoUTF8(dadosVenda);

  IniciaComando;
  try
    if fsConfigArquivos.SalvarEnvio then
    begin
      NomeCFe := CalcCFeNomeArq( fsConfigArquivos.PastaEnvio,
                                 fsConfigArquivos.PrefixoArqCFe +
                                 FormatDateTime('YYYYMMDDHHNNSS',Now) + '-' +
                                 IntToStrZero(numeroSessao, 6),
                                 '-env');
      WriteToTXT(NomeCFe, dadosVenda, False, False);
      DoLog('  Gravando XML Venda enviado: '+NomeCFe);
    end;

    DoLog( '  Inicio do Envio');
    SATResp := '';
    tini := now;
    if assigned(fsOnEnviarDadosVenda) then
      fsOnEnviarDadosVenda(dadosVenda, SATResp);

    if EstaVazio(SATResp) then
      SATResp := fsSATClass.EnviarDadosVenda( dadosVenda );

    tfim := now;
    DoLog( '  Tempo de Processamento: '+ FormatFloat('##0.000',SecondSpan(tini,tfim))+' segundos' );
  finally
    Result := FinalizaComando( SATResp );
  end;

  DecodificaRetorno6000;
end ;

procedure TACBrSAT.ExtrairLogs(const NomeArquivo: String);
var
  SL: TStringList;
begin
  if NomeArquivo = '' then
    raise EACBrSATErro.Create('Nome para Arquivo de Log não especificado');

  SL := TStringList.Create;
  try
    SL.Clear;
    ExtrairLogs(SL);
    SL.SaveToFile(NomeArquivo)
  Finally
    SL.Free;
  end;
end ;

procedure TACBrSAT.ExtrairLogs(AStringList: TStrings);
var
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    MS.Clear;
    ExtrairLogs(MS);
    MS.Seek(0,soBeginning);
    AStringList.LoadFromStream(MS);
  Finally
    MS.Free;
  end;
end;

procedure TACBrSAT.ExtrairLogs(AStream: TStream);
var
  LogBin : AnsiString;
  SATResp: String;
begin
  fsComandoLog := 'ExtrairLogs';
  IniciaComando;
  try
    SATResp := '';
    if Assigned(fsOnExtrairLogs) then
      fsOnExtrairLogs(SATResp);

    if EstaVazio(SATResp) then
      SATResp := fsSATClass.ExtrairLogs;
  finally
    FinalizaComando( SATResp );
  end;

  // TODO: Criar verificação para os retornos: 15002, e 11098 - SAT em processamento

  // Transformando retorno, de Base64 para ASCII
  if (Resposta.RetornoLst.Count > 5) and
     (Resposta.codigoDeRetorno = 15000) then  // 1500 = Transferência completa
  begin
    LogBin := DecodeBase64( Resposta.RetornoLst[5] );

    AStream.Size := 0;
    WriteStrToStream(AStream, LogBin);
  end;
end;

function TACBrSAT.TesteFimAFim(const dadosVenda : AnsiString) : String ;
var
  XMLRecebido, NomeCFe , SATResp: String;
begin
  fsComandoLog := 'TesteFimAFim(' +dadosVenda+' )';
  IniciaComando;
  try
    if fsConfigArquivos.SalvarEnvio then
    begin
      NomeCFe := CalcCFeNomeArq( fsConfigArquivos.PastaEnvio,
                                 fsConfigArquivos.PrefixoArqCFe +
                                 FormatDateTime('YYYYMMDDHHNNSS',Now) + '-' +
                                 IntToStrZero(numeroSessao, 6),
                                 '-teste-env');
      WriteToTXT(NomeCFe, dadosVenda, False, False);
      DoLog('  Gravando XML TesteFimAFim enviado: '+NomeCFe);
    end;

    SATResp := fsSATClass.TesteFimAFim( dadosVenda );
  finally
    Result := FinalizaComando( SATResp );
  end;

  if fsResposta.codigoDeRetorno = 9000 then
  begin
     XMLRecebido := DecodeBase64(fsResposta.RetornoLst[5]);
     CFe.AsXMLString := XMLRecebido;

     if fsConfigArquivos.SalvarCFe then
     begin
       NomeCFe := CalcCFeNomeArq(fsConfigArquivos.PastaCFeVenda,'','-teste');
       CFe.SaveToFile(NomeCFe);
       DoLog('  Gravando XML TesteFimAFim recebido: '+NomeCFe);
     end;
  end;
end ;

function TACBrSAT.TrocarCodigoDeAtivacao(const codigoDeAtivacaoOuEmergencia: AnsiString;
  opcao: Integer; const novoCodigo: AnsiString): String;
var
  SATResp: String;
begin
  fsComandoLog := 'TrocarCodigoDeAtivacao('+ codigoDeAtivacaoOuEmergencia+', '+
                  IntToStr(opcao)+ ', '+novoCodigo+' )';
  IniciaComando;
  try
    SATResp := fsSATClass.TrocarCodigoDeAtivacao( codigoDeAtivacaoOuEmergencia, opcao, novoCodigo );
  finally
    Result := FinalizaComando( SATResp );
  end;
end ;

function TACBrSAT.ValidarDadosVenda(dadosVenda: AnsiString; out msgErro: String
  ): Boolean;
begin
  fsComandoLog := 'ValidarDadosVenda( '+dadosVenda+' )';

  if EstaVazio(fsConfig.ArqSchema) then
    raise EACBrSATErro.Create('Config.ArqSchema não informado');

  dadosVenda := Trim(dadosVenda);

  if dadosVenda = '' then
     raise EACBrSATErro.Create('Parâmetro: "dadosVenda" não informado');

  SSL.SSLXmlSignLib := fsConfig.XmlSignLib;

  msgErro := '';
  Result := SSL.Validar(dadosVenda, fsConfig.ArqSchema, msgErro);
end;

function TACBrSAT.GetcodigoDeAtivacao : AnsiString ;
var
  AcodigoDeAtivacao : AnsiString ;
begin
  AcodigoDeAtivacao := '';

  if Assigned( fsOnGetcodigoDeAtivacao ) then
     fsOnGetcodigoDeAtivacao( AcodigoDeAtivacao ) ;

  Result := AcodigoDeAtivacao;
end;

function TACBrSAT.GetModeloStrClass : String ;
begin
   Result := fsSATClass.ModeloStr;
end;

function TACBrSAT.GetNomeModeloCFe: String;
begin
  Result := fsPrefixoCFe;
end;

function TACBrSAT.GetsignAC : AnsiString ;
var
  AsignAC : AnsiString ;
begin
  AsignAC := '';

  if Assigned( fsOnGetsignAC ) then
     fsOnGetsignAC( AsignAC ) ;

  Result := AsignAC;
end;

procedure TACBrSAT.SetInicializado(AValue : Boolean) ;
begin
  if AValue then
    Inicializar
  else
    DesInicializar ;
end ;

procedure TACBrSAT.SetMAIL(const AValue: TACBrMail);
begin
  if AValue <> fsMAIL then
  begin
    if Assigned(fsMAIL) then
      fsMAIL.RemoveFreeNotification(Self);

    fsMAIL := AValue;

    if AValue <> nil then
      AValue.FreeNotification(self);
  end;
end;

procedure TACBrSAT.SetIntegrador(AValue: TACBrIntegrador);
begin
  if AValue <> fsIntegrador then
  begin
    if Assigned(fsIntegrador) then
      fsIntegrador.RemoveFreeNotification(Self);

    fsIntegrador := AValue;

    if AValue <> nil then
      AValue.FreeNotification(self);
  end;
end;

procedure TACBrSAT.SetModelo(AValue : TACBrSATModelo) ;
var
  wArqLOG : String ;
begin
  if fsModelo = AValue then exit ;

  if fsInicializado then
    raise EACBrSATErro.Create( cACBrSATSetModeloException );

  if (AValue = mfe_Integrador_XML) and (not Assigned(fsIntegrador)) then
    raise EACBrSATErro.Create( cACBrSATSemIntegrador );

  wArqLOG := ArqLOG ;

  FreeAndNil( fsSATClass ) ;

  { Instanciando uma nova classe de acordo com AValue }
  case AValue of
    satDinamico_cdecl : fsSATClass := TACBrSATDinamico_cdecl.Create( Self ) ;
    satDinamico_stdcall : fsSATClass := TACBrSATDinamico_stdcall.Create( Self ) ;
    mfe_Integrador_XML : fsSATClass := TACBrSATMFe_integrador_XML.Create( Self ) ;
  else
    fsSATClass := TACBrSATClass.Create( Self ) ;
  end;

  { Passando propriedades da Classe anterior para a Nova Classe }
  ArqLOG := wArqLOG ;

  fsModelo := AValue;
end ;

procedure TACBrSAT.SetNomeDLL(const AValue : string) ;
var
  FileName: String;
begin
  if fsNomeDLL = AValue then Exit ;
  fsNomeDLL := Trim(AValue) ;

  FileName := ExtractFileName( fsNomeDLL );
  if FileName = '' then
    fsNomeDLL := PathWithDelim( fsNomeDLL ) + cLIBSAT;
end ;

procedure TACBrSAT.SetAguardandoResposta(AValue: Boolean);
begin
  if fsAguardandoResposta = AValue then
    Exit;

  fsAguardandoResposta := AValue;
  if Assigned(fsOnAguardandoRespostaChange) then
    fsOnAguardandoRespostaChange(Self);
end;

procedure TACBrSAT.SetExtrato(const Value: TACBrSATExtratoClass);
Var
  OldValue: TACBrSATExtratoClass ;
begin
  if Value <> fsExtrato then
  begin
     if Assigned(fsExtrato) then
        fsExtrato.RemoveFreeNotification(Self);

     OldValue  := fsExtrato ;   // Usa outra variavel para evitar Loop Infinito
     fsExtrato := Value;    // na remoção da associação dos componentes

     if Assigned(OldValue) then
        if Assigned(OldValue.ACBrSAT) then
           OldValue.ACBrSAT := nil ;

     if Value <> nil then
     begin
        Value.FreeNotification(self);
        Value.ACBrSAT := self ;
     end ;
  end ;
end;

procedure TACBrSAT.Notification(AComponent : TComponent ; Operation : TOperation
  ) ;
begin
  inherited Notification(AComponent, Operation) ;

  if (Operation = opRemove) then
  begin
    if (fsExtrato <> Nil) and (AComponent is TACBrSATExtratoClass) then
      fsExtrato := Nil

    else if (fsMAIL <> Nil) and (AComponent is TACBrMail) then
      fsMAIL := Nil

    else if (fsIntegrador <> Nil) and (AComponent is TACBrIntegrador) then
      fsIntegrador := Nil;
  end;
end ;

procedure TACBrSAT.DecodificaRetorno6000;
var
  XMLRecebido: String;
  NomeCFe: String;
begin
  if fsResposta.codigoDeRetorno <> 6000 then exit;

  XMLRecebido := DecodeBase64(fsResposta.RetornoLst[6]);
  // Se não tem Declaracao no XML, insere a padrão. Retorno sempre deve ser em UTF8
  if ObtemDeclaracaoXML(XMLRecebido) = '' then
    XMLRecebido := CUTF8DeclaracaoXML + XMLRecebido;

  CFe.AsXMLString := XMLRecebido;

  if fsConfigArquivos.SalvarCFe then
  begin
    NomeCFe := CalcCFeNomeArq(fsConfigArquivos.PastaCFeVenda);
    CFe.SaveToFile(NomeCFe);
    DoLog('  Gravando XML Venda recebido: '+NomeCFe);
  end;
end;

procedure TACBrSAT.DecodificaRetorno7000;
var
  XMLRecebido: String;
  NomeCFe: String;
begin
  if fsResposta.codigoDeRetorno <> 7000 then exit;

  XMLRecebido := DecodeBase64(fsResposta.RetornoLst[6]);
  CFeCanc.AsXMLString := XMLRecebido;

  if fsConfigArquivos.SalvarCFeCanc then
  begin
    NomeCFe := CalcCFeCancNomeArq(fsConfigArquivos.PastaCFeCancelamento);
    CFeCanc.SaveToFile(NomeCFe);
    DoLog('  Gravando XML Cancelamento recebido: '+NomeCFe);
  end;
end;


procedure TACBrSAT.CFe2CFeCanc;
begin
  CFeCanc.Clear;
  CFeCanc.infCFe.versao   := CFe.infCFe.versao;
  CFeCanc.infCFe.chCanc   := fsPrefixoCFe + CFe.infCFe.ID;
  CFeCanc.infCFe.dEmi     := CFe.ide.dEmi;
  CFeCanc.infCFe.hEmi     := CFe.ide.hEmi;
  CFeCanc.ide.CNPJ        := CFe.ide.CNPJ;
  CFeCanc.ide.signAC      := CFe.ide.signAC;
  CFeCanc.ide.numeroCaixa := CFe.ide.numeroCaixa;
  CFeCanc.Dest.CNPJCPF    := CFe.Dest.CNPJCPF;
end;

procedure TACBrSAT.ImprimirExtrato;
begin
  VerificaCondicoesImpressao;
  Extrato.ImprimirExtrato;
end;

procedure TACBrSAT.ImprimirExtratoResumido;
begin
  VerificaCondicoesImpressao;
  Extrato.ImprimirExtratoResumido;
end;

procedure TACBrSAT.ImprimirExtratoCancelamento;
begin
  VerificaCondicoesImpressao( True );
  Extrato.ImprimirExtratoCancelamento;
end;

function TACBrSAT.CalcCFeNomeArq(const Pasta: String; NomeArquivo: String;
  const Sufixo: String; const Extensao: String): String;
var
  Dir: String;
begin
  Dir := fsConfigArquivos.CalcPath( Pasta, CFe.Emit.CNPJ, CFe.ide.dEmi );

  if not DirectoryExists(Dir) then
    ForceDirectories(Dir);

  if NomeArquivo = '' then
    NomeArquivo := fsConfigArquivos.PrefixoArqCFe + CFe.infCFe.ID;

  Result := Dir + NomeArquivo + Sufixo + Extensao;
end;

function TACBrSAT.CalcCFeCancNomeArq(const Pasta: String; NomeArquivo: String;
  const Sufixo: String; const Extensao: String): String;
var
  Dir, Chave: String;
begin
  Dir := fsConfigArquivos.CalcPath( Pasta, CFeCanc.Emit.CNPJ, CFeCanc.ide.dEmi );

  if not DirectoryExists(Dir) then
    ForceDirectories(Dir);

  if NomeArquivo = '' then
  begin
    Chave := CFeCanc.infCFe.ID;
    if Chave = '' then
      Chave := OnlyNumber( CFeCanc.infCFe.chCanc );

    NomeArquivo := fsConfigArquivos.PrefixoArqCFeCanc + Chave;
  end;

  Result := Dir + NomeArquivo + Sufixo + Extensao;
end;

function TACBrSAT.CodificarPaginaDeCodigoSAT(const ATexto: String): AnsiString;
begin
  if fsConfig.PaginaDeCodigo > 0 then
     Result := TranslateString( ACBrStrToAnsi( ATexto ), fsConfig.PaginaDeCodigo )
  else
     Result := TiraAcentos( ATexto );
end ;

function TACBrSAT.DecodificarPaginaDeCodigoSAT(const ATexto : AnsiString
   ) : String ;
begin
  if fsConfig.PaginaDeCodigo > 0 then
     Result := ACBrStr( TranslateString( ATexto, 0, fsConfig.PaginaDeCodigo ) )
  else
     Result := ACBrStr( ATexto ) ;
end ;

function TACBrSAT.GravarStream(AStream: TStream): Boolean;
begin
  Result := False;

  if EstaVazio(CFe.XMLOriginal) then
    CFe.GerarXML;

  AStream.Size := 0;
  WriteStrToStream(AStream, AnsiString(CFe.XMLOriginal));
  Result := True;
end;

function TACBrSAT.VerificarVersaoSAT(const VersaoLayout: Double): Double;
begin
  ConsultarStatusOperacional;
  if (Resposta.codigoDeRetorno <> 10000) then
    raise EACBrSATErro.Create('VerificarVersaoSAT: Erro ao ConsultarStatusOperacional');

  Result := StringToFloatDef(Status.VER_LAYOUT, -1);
  if Result <= 0 then
    raise EACBrSATErro.Create('VerificarVersaoSAT: Falha ao ler versão do SAT');

  if (VersaoLayout > 0) and (VersaoLayout > Result) then
    raise EACBrSATErro.CreateFmt('VerificarVersaoSAT: SAT não suporta a versão [%f]', [VersaoLayout]);
end;

procedure TACBrSAT.EnviarEmail(const sPara, sAssunto: String; const NomeArq: String;
  sMensagem: TStrings; sCC: TStrings; Anexos: TStrings; StreamCFe: TStream);
var
  i: Integer;
begin
  if not Assigned(MAIL) then
    raise EACBrSATErro.Create('Componente ACBrMail não associado');

  MAIL.Clear;
  MAIL.AddAddress(sPara);
  MAIL.Subject := sAssunto;

  if Assigned(sMensagem) then
  begin
    MAIL.Body.Text := sMensagem.Text;
    MAIL.AltBody.Text := (StripHTML(sMensagem.Text));
  end;

  if Assigned(StreamCFe) then
    MAIL.AddAttachment(StreamCFe, NomeArq, adAttachment);

  if Assigned(Anexos) then
  begin
    for i := 0 to Anexos.Count - 1 do
      MAIL.AddAttachment(Anexos[i],'',AdAttachment);
  end;

  if Assigned(sCC) then
  begin
    for i := 0 to sCC.Count - 1 do
      MAIL.AddCC(sCC[i]);
  end;

  MAIL.Send;
end;

procedure TACBrSAT.EnviarEmail(const sPara, sAssunto: String;
  sMensagem: TStrings; sCC: TStrings; Anexos: TStrings);
var
  AnexosEmail:TStrings;
  StreamCFe : TMemoryStream;
begin
  if not Assigned(fsMAIL) then
    raise EACBrSATErro.Create('Componente ACBrMail não associado');

  AnexosEmail := TStringList.Create;
  StreamCFe   := TMemoryStream.Create;
  try
    AnexosEmail.Clear;
    if Assigned(Anexos) then
      AnexosEmail.Assign(Anexos);

    GravarStream(StreamCFe);
    EnviarEmail( sPara, sAssunto, 'AD' + CFe.infCFe.ID + '.xml', sMensagem, sCC, AnexosEmail, StreamCFe);
  finally
    AnexosEmail.Free;
    StreamCFe.Free;
  end;
end;

end.

