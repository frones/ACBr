{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: José M. S. Junior                               }
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

unit ACBrLibBoletoConsts;


interface

uses
  Classes, SysUtils, ACBrLibComum;

const
  CSessaoBoletoDiretorioConfig = 'BoletoDiretorioConfig';
  CChaveDataArquivo = 'DataArquivo';
  CChaveDataCreditoLanc = 'DataCreditoLanc';
  CChaveDirArqRemessa = 'DirArqRemessa';
  CChaveDirArqRetorno = 'DirArqRetorno';
  CChaveDirHomologacao = 'DirHomologacao';
  CChaveImprimirMensagemPadrao = 'ImprimirMensagemPadrao';
  CChaveLayoutRemessa = 'LayoutRemessa';
  CChaveLeCedenteRetorno = 'LeCedenteRetorno';
  CChaveLerNossoNumeroCompleto = 'LerNossoNumeroCompleto';
  CChaveNomeArqRemessa = 'NomeArqRemessa';
  CChaveNomeArqRetorno = 'NomeArqRetorno';
  CChaveNumeroArquivo = 'NumeroArquivo';
  CChaveRemoveAcentosArqRemessa = 'RemoveAcentosArqRemessa';
  CChavePrefixArqRemessa = 'PrefixArqRemessa';

  CSessaoBoletoBancoConfig = 'BoletoBancoConfig';
  CChaveDigitoBanco = 'Digito';
  CChaveLayoutVersaoArquivo = 'LayoutVersaoArquivo';
  CChaveLayoutVersaoLote = 'LayoutVersaoLote';
  CChaveLocalPagamento = 'LocalPagamento';
  CChaveNumero = 'Numero';
  CChaveNumeroCorrespondente = 'NumeroCorrespondente';
  CChaveOrientacaoBanco = 'OrientacaoBanco';
  CChaveTipoCobranca = 'TipoCobranca';
  CChaveCasasDecimaisMoraJuros = 'CasasDecimaisMoraJuros';
  CChaveCIP = 'CIP';

  CSessaoBoletoCedenteConfig = 'BoletoCedenteConfig';
  CChaveAgencia= 'Agencia';
  CChaveAgenciaDigito= 'AgenciaDigito';
  CChaveBairro= 'Bairro';
  CChaveCaracTitulo= 'CaracTitulo';
  CChaveCEP= 'CEP';
  CChaveCidade= 'Cidade';
  CChaveCNPJCPF= 'CNPJCPF';
  CChaveCodigoCedente= 'CodigoCedente';
  CChaveCodigoTransmissao= 'CodigoTransmissao';
  CChaveComplemento= 'Complemento';
  CChaveConta= 'Conta';
  CChaveContaDigito= 'ContaDigito';
  CChaveConvenio= 'Convenio';
  CChaveLogradouro= 'Logradouro';
  CChaveModalidade= 'Modalidade';
  CChaveNome= 'Nome';
  CChaveNumeroRes= 'NumeroRes';
  CChaveResponEmissao= 'ResponEmissao';
  CChaveTelefone= 'Telefone';
  CChaveTipoCarteira= 'TipoCarteira';
  CChaveTipoDocumento= 'TipoDocumento';
  CChaveTipoInscricao= 'TipoInscricao';
  CChaveUF= 'UF';
  CChaveDigitoVerificadorAgenciaConta= 'DigitoVerificadorAgenciaConta';
  CChaveIdentDistribuicao= 'IdentDistribuicao';
  CChaveOperacao= 'Operacao';
  CChavePIX = 'ChavePIX';
  CTipoChavePix = 'TipoChavePIX';

  CSessaoBoletoFCFortesConfig = 'BoletoBancoFCFortesConfig';
  CChaveDirLogo= 'DirLogo';
  CChaveFiltro= 'Filtro';
  CChaveLayout= 'Layout';
  CChaveMostrarPreview= 'MostrarPreview';
  CChaveMostrarProgresso= 'MostrarProgresso';
  CChaveMostrarSetup= 'MostrarSetup';
  CChaveNomeArquivo= 'NomeArquivo';
  CChaveNumeroCopias= 'NumeroCopias';
  CChavePrinterName= 'PrinterName';
  CChaveSoftwareHouse= 'SoftwareHouse';
  CChaveAlterarEscalaPadrao= 'AlterarEscalaPadrao';
  CChaveNovaEscala= 'NovaEscala';
  CChaveMargemInferior='MargemInferior';
  CChaveMargemSuperior='MargemSuperior';
  CChaveMargemEsquerda='MargemEsquerda';
  CChaveMargemDireita ='MargemDireita';

  CSessaoBoletoCedenteWS = 'BoletoCedenteWS';
  CChaveClientID= 'ClientID';
  CChaveClientSecret= 'ClientSecret';
  CChaveKeyUser= 'KeyUser';
  CChaveScope= 'Scope';
  CChaveIndicadorPix= 'IndicadorPix';

  CSessaoBoletoWebService = 'BoletoWebSevice';
  CChaveLogRegistro = 'LogRegistro';
  CChavePathGravarRegistro = 'PathGravarRegistro';
  CChaveAmbiente = 'Ambiente';
  CChaveArquivoCRT = 'ArquivoCRT';
  CChaveArquivoKEY = 'ArquivoKEY';
  CChaveVersaoDF = 'VersaoDF';
  CChaveUseCertificateHTTP = 'UseCertificateHTTP';
  CChaveSSLType = 'SSLType';
  CChaveTimeout = 'Timeout';

  CSessaoBolConfig = 'BoletoConfig';
  CChaveemailAssuntoBoleto= 'emailAssuntoBoleto';
  CChaveemailMensagemBoleto= 'emailMensagemBoleto';

  CSessaoRetorno = 'Retorno';
  CSessaoCedente = 'CEDENTE';
  CSessaoBanco = 'BANCO';
  CSessaoConta = 'CONTA';
  CSessaoTitulo = 'TITULO';
  CSessaoMotivoRejeicao = 'MotivoRejeicao';

  CSessaoRegistro = 'REGISTRO';
  CSessaoTituloRetorno = 'TITULORETORNO';
  CSessaoRejeicao = 'REJEICAO';
  CSessaoSacado = 'Sacado';
  CSessaoSacadoAvalista = 'SacadoAvalista';

Resourcestring
  SInfBoletosCarregados = '%d Boletos(s) Carregado(s)';
  SErroLerArquivoEntrada ='Erro ao ler arquivo de entrada: %d';

function SetRetornoBoletosCarregados(const libHandle: PLibHandle; const NumBoleto: Integer): Integer;


implementation

function SetRetornoBoletosCarregados(const libHandle: PLibHandle; const NumBoleto: Integer): Integer;
begin
  Result := libHandle^.Lib.SetRetorno(0, {NumBoleto,} Format(SInfBoletosCarregados, [NumBoleto]))
end;

end.

