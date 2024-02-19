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

unit ACBrLibConsts;

interface

uses
  Classes, SysUtils, ACBrLibComum;

const
  CACBrLib = 'ACBrLib';
  CACBrLibVersaoConfig = '0.0.2';
  CLibChaveCrypt = 'tYk*5W@';
  CLibMemory = '[Memory]';

  {$IfDef MSWINDOWS}
  CNomeArqConf = 'ACBrLib.ini';
  {$Else}
  CNomeArqConf = 'acbrlib.ini';
  {$EndIf}

  CSessaoPrincipal = 'Principal';
  CSessaoVersao = 'Versao';
  CChaveChave = 'Chave';
  CChaveTipoResposta = 'TipoResposta';
  CChaveCodificacaoResposta = 'CodificacaoResposta';
  CChaveLogNivel = 'LogNivel';
  CChaveLogPath = 'LogPath';
  CChaveTimeOut = 'Timeout';
  CChaveNome = 'Nome';
  CChaveServidor = 'Servidor';
  CChaveUsuario = 'Usuario';
  CChaveSenha = 'Senha';
  CChavePorta = 'Porta';
  CChaveDevice = 'Device';

  CSessaoConsultaCNPJ = 'ConsultaCNPJ';

  CSessaoSistema = 'Sistema';
  CChaveVersao = 'Versao';
  CChaveDescricao = 'Descricao';
  CChaveData = 'Data';

  CSessaoProxy = 'Proxy';
  CSessaoEmail = 'Email';
  CChaveEmailConta = 'Conta';
  CChaveEmailCodificacao = 'Codificacao';
  CChaveEmailSSL = 'SSL';
  CChaveEmailTLS = 'TLS';
  CChaveEmailSSLType = 'SSLType';
  CChaveEmailConfirmacao = 'Confirmacao';
  CChaveEmailConfirmacaoEntrega = 'ConfirmacaoEntrega';
  CChaveEmailSegundoPlano = 'SegundoPlano';
  CChaveEmailTentativas = 'Tentativas';
  CChaveEmailIsHTML = 'IsHTML';
  CChaveEmailPriority = 'Priority';

  CSessaoSwHouse = 'SoftwareHouse';
  CSessaoEmissor = 'Emissor';

  CChaveNomeFantasia = 'NomeFantasia';
  CChaveRazaoSocial = 'RazaoSocial';
  CChaveCNPJ = 'CNPJ';
  CChaveWebSite = 'WebSite';
  CChaveEmail = 'Email';
  CChaveTelefone = 'Telefone';
  CChaveResponsavel = 'Responsavel';

  CChavePathLogo = 'PathLogo';
  CChavePathPDF = 'PathPDF';
  CChaveUsaSeparadorPathPDF = 'UsaSeparadorPathPDF';
  CChaveNomeDocumento = 'NomeDocumento';
  CChaveImpressora = 'Impressora';
  CChaveTipoRelatorioBobina = 'TipoRelatorioBobina';
  CChaveMostraPreview = 'MostraPreview';
  CChaveMostraStatus = 'MostraStatus';
  CChaveMostraSetup = 'MostraSetup';
  CChaveCopias = 'Copias';
  CChaveMargemInferior = 'MargemInferior';
  CChaveMargemSuperior = 'MargemSuperior';
  CChaveMargemEsquerda = 'MargemEsquerda';
  CChaveMargemDireita = 'MargemDireita';
  CChaveAlterarEscalaPadrao = 'AlterarEscalaPadrao';
  CChaveNovaEscala = 'NovaEscala';
  CChaveExpandeLogoMarca = 'ExpandeLogoMarca';
  CChaveCalcularNomeArquivoPDFIndividual = 'CalcularNomeArquivoPDFIndividual';

  CChaveExpandeLogoMarcaAltura = 'ExpandeLogoMarca.Altura';
  CChaveExpandeLogoMarcaEsquerda = 'ExpandeLogoMarca.Esquerda';
  CChaveExpandeLogoMarcaTopo = 'ExpandeLogoMarca.Topo';
  CChaveExpandeLogoMarcaLargura = 'ExpandeLogoMarca.Largura';
  CChaveExpandeLogoMarcaDimensionar = 'ExpandeLogoMarca.Dimensionar';
  CChaveExpandeLogoMarcaEsticar = 'ExpandeLogoMarca.Esticar';

  CChaveCasasDecimaisFormato = 'CasasDecimais.Formato';
  CChaveCasasDecimaisMaskqCom = 'CasasDecimais.MaskqCom';
  CChaveCasasDecimaisMaskvUnCom = 'CasasDecimais.MaskvUnCom';
  CChaveCasasDecimaisqCom = 'CasasDecimais.qCom';
  CChaveCasasDecimaisvUnCom = 'CasasDecimais.vUnCom';
  CChaveCasasDecimaisMaskAliquota = 'CasasDecimais.MaskAliquota';
  CChaveCasasDecimaisAliquota = 'CasasDecimais.Aliquota';

  CSessaoIntegrador = 'Integrador';
  CChaveArqLog = 'ArqLog';
  CChavePastaInput = 'PastaInput';
  CChavePastaOutput = 'PastaOutput';

  CSessaoPosPrinter = 'PosPrinter';
  CSessaoPosPrinterBarras = 'PosPrinter_Barras';
  CSessaoPosPrinterQRCode = 'PosPrinter_QRCode';
  CSessaoPosPrinterLogo = 'PosPrinter_Logo';
  CSessaoPosPrinterGaveta = 'PosPrinter_Gaveta';
  CSessaoPosPrinterMPagina = 'PosPrinter_MPagina';
  CSessaoPosPrinterDevice = 'PosPrinter_Device';


  CChaveModelo = 'Modelo';
  CChavePaginaDeCodigo = 'PaginaDeCodigo';
  CChaveColunasFonteNormal = 'ColunasFonteNormal';
  CChaveEspacoEntreLinhas = 'EspacoEntreLinhas';
  CChaveLinhasEntreCupons = 'LinhasEntreCupons';
  CChaveCortaPapel = 'CortaPapel';
  CChaveTraduzirTags = 'TraduzirTags';
  CChaveIgnorarTags = 'IgnorarTags';
  CChaveLinhasBuffer = 'LinhasBuffer';
  CChaveControlePorta = 'ControlePorta';
  CChaveVerificarImpressora = 'VerificarImpressora';
  CChaveTipoCorte = 'TipoCorte';

  CChaveCBMostrarCodigo = 'MostrarCodigo';
  CChaveCBLarguraLinha = 'LarguraLinha';
  CChaveCBAltura = 'Altura';
  CChaveCBMargem = 'Margem';

  CChaveQRTipo = 'Tipo';
  CChaveQRLarguraModulo = 'LarguraModulo';
  CChaveQRErrorLevel = 'ErrorLevel';

  CChaveLGIgnorarLogo = 'IgnorarLogo';
  CChaveLGKeyCode1 = 'KeyCode1';
  CChaveLGKeyCode2 = 'KeyCode2';
  CChaveLGFatorX = 'FatorX';
  CChaveLGFatorY = 'FatorY';

  CChaveGVSinalInvertido = 'SinalInvertido';
  CChaveGVTempoON = 'TempoON';
  CChaveGVTempoOFF = 'TempoOFF';

  CChaveMPLargura = 'Largura';
  CChaveMPAltura = 'Altura';
  CChaveMPEsquerda = 'Esquerda';
  CChaveMPTopo = 'Topo';
  CChaveMPDirecao = 'Direcao';
  CChaveMPEspacoEntreLinhas = 'EspacoEntreLinhas';

  CSessaoDFe = 'DFe';
  CChaveSSLCryptLib = 'SSLCryptLib';
  CChaveArquivoPFX = 'ArquivoPFX';
  CChaveDadosPFX = 'DadosPFX';
  CChaveNumeroSerie = 'NumeroSerie';

  CChaveBaud = 'Baud';
  CChaveParity = 'Parity';
  CChaveStop = 'Stop';
  CChaveMaxBandwidth = 'MaxBandwidth';
  CChaveSendBytesCount = 'SendBytesCount';
  CChaveSendBytesInterval = 'SendBytesInterval';
  CChaveHandShake = 'HandShake';
  CChaveSoftFlow = 'SoftFlow';
  CChaveHardFlow = 'HardFlow';

  CSessaoRespRetorno = 'Retorno';
  CSessaoRespDistribuicaoDFe = 'DistribuicaoDFe';
  CSessaoRespConsultaCadastro = 'ConsultaCadastro';
  CSessaoRespArquivo = 'Arquivo';

resourcestring
  SErrLibSemNome = 'Nome da Biblioteca não foi definido';
  SErrLibDono = 'Dono de TLibConfig deve ser do tipo TACBrLib';

  SErrLibJaInicializada = '%s já inicializada.';
  SErrLibNaoInicializada = 'Biblioteca não foi inicializada';
  SErrLibNaoFinalizada = '%s não foi finalizada';
  SErrLibNaoCarregada = 'Biblioteca %s não pode ser carregada';

  SErrDiretorioInvalido = 'Diretório Invalido: %s';
  SErrConfSessaoNaoExiste = 'Sessão não [%s] existe no arquivo de configuração';
  SErrConfChaveNaoExiste = 'Chave [%s] não existe na Sessão [%s] no arquivo de configuração';

  SErrArquivoNaoExiste = 'Arquivo % não encontrado';
  SErrIndex = 'Indice informado % não encontrado';

  SErrRetornoHttpWebService = 'WebService %s, retorno http: %d';
  SErroDemoExpirado = 'O Demo da %s expirou';

const
{$I ACBrLibErros.inc}

function SetRetornoWebService(const libHandle: PLibHandle; const CodigoHTTP: Integer; const WebService: String; const Message: String = ''): Integer;
function GerarRetornoWebService(const libHandle: PLibHandle; const CodigoHTTP: Integer; const WebService: String; const Message: String = ''): String;

implementation
uses
  ACBrLibResposta, ACBrUtil.Strings;

function GerarRetornoWebService(const libHandle: PLibHandle; const CodigoHTTP: Integer; const WebService: String; const Message: String = ''): String;
Var
  Resp: TACBrLibHttpResposta;
begin
  Result := '';
  Resp := TACBrLibHttpResposta.Create(libHandle^.Lib.Config.TipoResposta, libHandle^.Lib.Config.CodResposta);
  try
    Resp.CodigoHTTP := CodigoHTTP;
    Resp.WebService := WebService;
    Resp.Msg := IfEmptyThen(Message, Format(SErrRetornoHttpWebService, [WebService, CodigoHTTP]));
    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function SetRetornoWebService(const libHandle: PLibHandle; const CodigoHTTP: Integer; const WebService: String; const Message: String = ''): Integer;
Var
  Resposta: String;
begin
  Resposta := GerarRetornoWebService(libHandle, CodigoHTTP, WebService, Message);
  Result := libHandle^.Lib.SetRetorno(ErrHttp, Resposta);
end;

end.

