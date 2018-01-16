{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2009   Isaque Pinheiro                      }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 23/08/2013: Juliana Tamizou
|*  - Padronização de componente existen no Branches de acordo com o ACBr
*******************************************************************************}
{$I ACBr.inc}

unit ACBrSEF2Conversao;

interface

Uses
  SysUtils, Classes, contnrs,
  ACBrTXTClass, pcnConversao;

  /// Código da finalidade do arquivo - TRegistro0000
  type TSEFIICodFinalidade = (raOriginal,     // 0 - Remessa do arquivo original
                              raSubstituto    // 1 - Remessa do arquivo substituto
                             );

  /// Versão do Leiaute do arquivo - TRegistro0000
  TSEFIIVersaoLeiaute = (
                         vlVersao2000  // Código 002 - Versão 2.0.0.0
                        );

  /// Tabela Conteúdo do Arquivo-texto - TRegistro0000
  TSEFIIConteudArquivo = (
                         caLancOpResultFiscal,  // Código 20 - Lançamentos de operações e resultados fiscais
                         caLancControlesFiscais, // Código 21 - Lançamentos de controles fiscais
                         caResumosInfConsolidadas, // Código 30 - Resumos e informações consolidadas
                         caExtratodocfiscais  // Código 91 - Lançamentos de controles fiscais 
                         );

  ///Indicador de Conteudo TRegistro001
  TSEFIIIndicadorConteudo = (
                             icContConteudo,//0- Documento com conteúdo
                             icSemConteudo // 1- Documento sem conteúdo
                             );

  ///Indicador de Conteudo TRegistroE001
  TSEFIIIndConteudoDocumento = (
                               idDocEntrSaiAjuste, // 0- Documento com lançamentos de entrada/aquisição, de saída/prestação e de ajuste
                               idDocSemLancamento, // 1- Documento sem lançamentos e ajustes
                               idDocEntrAjuste,    // 2- Documento com lançamentos de entrada/aquisição e de ajuste
                               idDocSaiAjuste,     // 3- Documento com lançamentos de saída/prestação e de ajuste
                               idDocLancAjuste     // 4- Documento com lançamentos de ajuste
                               );

  /// Tabela Externa 3.3.1 Tabela de Qualificação de Assinantes TRegistro005
  TSEFIIQualiAssinante = (
                  qaDiretor, //203	Diretor
                  qaConsAdministracao, //204	Conselheiro de administração
                  qaAdministrador, //205	Administrador
                  qaAdmGrupo, //206	Administrador de grupo
                  qaAdmSociedadeFiliada, //207	Administrador de sociedade filiada
                  qaAdmJudicialPessoaFisica, //220	Administrador judicial - pessoa física
                  qaAdmJudicialPessoaJuridica, //222	Administrador judicial - pessoa jurídica/profissional responsável
                  qaAdmJudicial, //223	Administrador judicial - gestor
                  qaGestorJudicial, //226	Gestor judicial
                  qaProcurador, //309	Procurador
                  qaInventariante, //312	Inventariante
                  qaLiquidante, //313	Liquidante
                  qaInterventor, //315	Interventor
                  qaEmpresario, //801	Empresário
                  qaContador, //900	Contador
                  qaOutros //999	Outros
                  );

  /// 6.1.1 - 	Tabela de Benefícios Fiscais do ICMS - TRegistro0025
  TSEFIIBeniFiscalICMS = (bfNenhum, //Nenhum beneficio fiscal
                          bfProdepe //PE001	Programa de Desenvolvimento do Estado de Pernambuco - Prodepe
                         );

  //Indicador de entrada de dados: TRegistro0030
  TSEFIIindicadorEntDados = (
                            iedDigitacaoDados,
                            iedImportacaoArquivo,
                            iedValidacaoArqTexto,
                            iedExportacao
                            );

  //Indicador do documento contido no arquivo:: TRegistro0030
  TSEFIIindicadorDocArquivo = (
                            daDocOriginalEmitArquivo,  //0- Documento original emitido em arquivo
                            daTranscDocEmissaoPropria,  //1- Transcrição de documentos de emissão própria
                            daTranscDocEmitTerceiros,  //2- Transcrição de documentos emitidos por terceiros
                            daTranscDocCaptDigitacao,  //3- Transcrição de documentos capturados por digitalização
                            daTranscDocEmitEquipEspecializado,  //4- Transcrição de documentos emitidos em equipamento especializado
                            daLivrosResultObrigacoes,  //5- Livros de resultados e obrigações
                            daLivrosMapasControle,  //6- Livros e mapas de controle
                            daGuiaInfoEconomFiscais,  //7- Guias de informações econômico-fiscais
                            daLivrosContabilidade,  //8- Livros da contabilidade
                            daExtratosDocumentos  //9- Extratos de documentos
                            );

  //Indicador de exigibilidade da escrituração do ISS: TRegistro0030
  TSEFIIindicadorExigEscriISS = (
                                issRegSimpEscrituraImposto,//0- Sim, com regime simplificado de escrituração do imposto
                                issRegimeIntEscrituApuracaoImposto,//2- Sim, com regime integral de escrituração e apuração do imposto
                                issNaoObrigadoEscriturar//9- Não obrigado a escriturar
                                  );

  ///Indicador de exigibilidade da escrituração do ICMS: TRegistro0030
  TSEFIndicadorExigEscriICMS = (
                                eiRegSimpEscImposto,//0- Sim, com regime simplificado de escrituração do imposto
                                eiRegIntermEscApuracaoNormal,//1- Sim, com regime intermediário de escrituração e apuração normal do imposto
                                eiRegimeIntEscApuracao,//2- Sim, com regime integral de escrituração e apuração normal do imposto
                                eiNaoObrigadoEscriturar//9- Não obrigado a escriturar
                              );

  ///Indicador de exigibilidade do Registro de Impressão de Documentos Fiscais: TRegistro0030
  TSEFIIIndicadorExigRegImpDocFiscais =(
                                        idSim,
                                        idNao,
                                        idVazio
                                       );

  //Indicador de exigibilidade do Registro de Utilização de Documentos Fiscais: TRegistro0030
  TSEFIIIndicadorExigRegUtiDocFiscais = (
                                         ufSim,
                                         ufNao
                                        );
  //Indicador de exigibilidade do Livro de Movimentação de Combustíveis: TRegistro0030
  TSEFIIIndicadorExigLivMovCombustiveis = (
                                          mcSim,
                                          mcNao
                                          );

  ///Indicador de exigibilidade do Registro de Veículos: TRegistro0030
  TSEFIIIndicadorExigRegVeiculos = (
                                    rvSim,
                                    rvNao
                                   );

  ///Indicador de exigibilidade anual do Registro de Inventário: TRegistro0030
  TSEFIIIndicadorExigRegAnualInvent = (
                                       aiSim,
                                       aiNao
                                      );

  //Indicador de apresentação da escrituração contábil: TRegistro0030
  TSEFIIIndicadorExigEscContabil = (
                                    ecCompRegArqDigital,//0- Completa registrada em arquivo digital
                                    ecCompRegPapelMFA,//1- Completa registrada em papel, microfilme, fichas avulsas, ou fichas/folhas contínuas
                                    ecSimpRegArqDigial,//2- Simplificada registrada em arquivo digital
                                    ecSimpRegPapelMFA,//3- Simplificada registrada papel, microfilme, fichas avulsas, ou fichas/folhas contínuas
                                    ecLivroCaixaRegArqDigial,//4- Livro Caixa registrado em arquivo digital
                                    ecLivroCaixaRegPapelMFA,//5- Livro Caixa registrado papel, microfilme, fichas avulsas, ou fichas/folhas contínuas
                                    ecNaoObrigadoEscriturar//9- Não obrigado a escriturar
                                   );
  //Indicador de operações sujeitas ao ISS: TRegistro0030
  TSEFIIIndicadorOpSujISS = (
                             issSim,
                             issNao
                            );

  //Indicador de operações sujeitas à retenção tributária do ISS, na condição de contribuinte-substituído: TRegistro0030
  TSEFIIIndicadorOpSujRetTribISSSub = (
                                       issSubSim,
                                       issSubNao
                                      );

  //Indicador de operações sujeitas ao ICMS:
  TSEFIIIndicadorOpSujICMS = (
                              sicmsSim,
                              sicmsNao
                             );

  //Indicador de operações sujeitas à substituição tributária do ICMS, na condição de contribuinte-substituto: TRegistro0030
  TSEFIIIndicadorOpSujSTICMSContributSub = (
                                stSim,
                                stNao
                               );

  //Indicador de operações sujeitas à antecipação tributária do ICMS, nas entradas: TRegistro0030
  TSEFIIIndicadorOpSujSTICMSEntradas = (
                                steSim,
                                steNao
                                      );

  //Indicador de operações sujeitas ao IPI: TRegistro0030
  TSEFIIIndicadorOpSujIPI = (
                                ipiSim,
                                ipiNao
                                      );
                           
  //Indicador de apresentação avulsa do Registro de Inventário: TRegistro0030
  TSEFIndicadorRegIvent = (
                           riSim,
                           riNao
                          );


 TSEFIIIndicadorDocArregadacao = (
                                 daEstadualDistrital,
                                 daGuiaNasRecEstadual,
                                 daDocArrecadacaoMunicipal,
                                 daDocArrecadacaoFederal,
                                 daOutros
                                 );



 TSEFIIDocFiscalReferenciado = (
                              SrefNF,        //01    - Nota Fiscal (NF)	1/1-A
                              SrefNFVCCVC,   //02    - Nota Fiscal de Venda a Consumidor (NFVC – ou CVC, se emitida por ECF)	2
                              SrefCCF,       //2D    - Cupom Fiscal, emitido por ECF (CCF)	-
                              SrefCBP,       //2E    - Bilhete de Passagem, emitido por ECF (CBP)	-
                              SrefNFPR,      //04    - Nota Fiscal de Produtor (NFPR)	4
                              SrefNFEE,      //06    - Nota Fiscal/Conta de Energia Elétrica (NFEE)	6
                              SrefNFTR,      //07    - Nota Fiscal de Serviço de Transporte (NFTR)	7
                              SrefCTRC,      //08    - Conhecimento de Transporte Rodoviário de Cargas (CTRC)	8
                              SrefCTAQ,      //09    - Conhecimento de Transporte Aquaviário de Cargas (CTAQ)	9
                              SrefCTAR,      //10    - Conhecimento Aéreo (CTAR)	10
                              SrefCTFC,      //11    - Conhecimento de Transporte Ferroviário de Cargas (CTFC)	11
                              SrefBPR,       //13    - Bilhete de Passagem Rodoviário (BPR)	13
                              SrefBPAQ,      //14    - Bilhete de Passagem Aquaviário (BPAQ)	14
                              SrefBPNB,      //15    - Bilhete de Passagem e Nota de Bagagem (BPNB)	15
                              SrefBPF,       //16    - Bilhete de Passagem Ferroviário (BPF)	16
                              SrefDT,        //17    - Despacho de Transporte (DT)	17
                              SrefRMD,       //18    - Resumo de Movimento Diário (RMD)	18
                              SrefOCC,       //20    - Ordem de Coleta de Cargas (OCC)	20
                              SrefNFSC,      //21    - Nota Fiscal de Serviço de Comunicação (NFSC)	21
                              SrefNFST,      //22    - Nota Fiscal de Serviço de Telecomunicação (NFST)	22
                              SrefGNRE,      //23    - Guia Nacional de Recolhimento Estadual (GNRE)	23
                              SrefACT,       //24    - Autorização de Carregamento e Transporte (ACT)	24
                              SrefMC,        //25    - Manifesto de Carga (MC)	25
                              SrefCTMC,      //26    - Conhecimento de Transporte Multimodal de Cargas (CTMC)	26
                              SrefNFTF,      //27    - Nota Fiscal de Serviço de Transporte Ferroviário (NFTF)	27
                              SrefNFGC,      //28    - Nota Fiscal/Conta de Fornecimento de Gás Canalizado (NFGC)	-
                              SrefNFAC,      //29    - Nota Fiscal/Conta de Fornecimento de Água Canalizada (NFAC)	-
                              SrefMV,        //30    - Manifesto de Vôo (MV)	-
                              SrefBRP,       //31    - Bilhete/Recibo do Passageiro (BRP)	-
                              SrefNFe,       //55    - Nota Fiscal Eletrônica (NF-e)	55
                              SrefCTe,       //57    - Conhecimento de Transporte Eletrônico (CT-e)	57
                              SrefNFCe,      //65    -
                              Sref98,        //98    -
                              Sref99         //98    -
                              );

   TIndiceOperacao = (SefioEntrada, SefioSaida);
   TIndiceEmissao = (SefiePropria, SefieTerceiros);
   TCodigoSituacao = (
                      SefcsEmissaonormal,             //00
                      SefcsEmissaocontingencia,       //01
                      SefcsEmissaocontingenciaFS,     //02
                      SefcsEmissaocontingenciaSCAN,   //03
                      SefcsEmissaocontingenciaDPEC,   //04
                      SefcsEmissaocontingenciaFSDA,   //05
                      SefcsEmissaoContingenciaSVCAN,  //06
                      SefcsEmissaoContingenciaSVCRS,  //07
                      SefcsEmissaoavulsa,             //10
                      SefcsComplemento,               //20
                      SefcsConsolidavalores,          //25
                      SefcsAutorizadenegada,          //80
                      SefcsNumerainutilizada,         //81
                      SefcsOperacancelada,            //90
                      SefcsNegociodesfeito,           //91
                      SefcsAjusteinformacoes,         //95
                      SefcsSemrepercussaofiscal       //99
                      );

    TIndicePagamento = (SefNenhum,SefipAVista, SefAPrazo, SefNaoOnerada);

    TIndicadorDados = (entDigitacao,entImportacao,entValidacao);

    TIndicadorDocArquivo = (arqOriginal,
                            arqTranscricaoEmissaoPropria,
                            arqTranscricaoEmissaoTerceiros,
                            arqTrancricaoDigitalizacao,
                            arqTranscricaoEmissaoEquipEspecilizado,
                            arqLivrosResultadosObrigacoes,
                            arqLivroMapaControles,
                            arqGuiasInfEconomicasFiscais,
                            arqLivrosContabilidade,
                            arqExtratoDocumentos);


    TIndicadorExigeEscrImposto = (impSimRegimeSimplificado,
                                  impSimRegimeIntermediario,
                                  impSimRegimeIntegral,
                                  impNaoObrigado);

    TIndicadorExigeDiversas = (exSim,exNao,exVazio);

    TIndicadorEscrContabil = (esCompletaArquivo,
                              esCompletaPapel,
                              esSimplificadaArquivo,
                              esSimplificadaPapel,
                              esLivroCaixaArquivo,
                              esLivroCaixaPapel,
                              esNaoObrigado,
                              esVazio);

    { TOpenBlocos }

    TOpenBlocos = class
    private
      fCOD_MUN: Integer;
      fIND_MOV: TSEFIIIndicadorConteudo;    /// Indicador de movimento: 0- Bloco com dados informados, 1- Bloco sem dados informados.
    public
      property COD_MUN: Integer read fCOD_MUN write fCOD_MUN;
      property IND_MOV: TSEFIIIndicadorConteudo read fIND_MOV write fIND_MOV;
    end;

    TACBrSEFIIRegistros = class(TObjectList)
    public
      function AchaUltimoPai(ANomePai, ANomeFilho: String): Integer;
    end;

    TACBrSEFIIEDOC = class(TACBrTXTClass)
    private
      FDT_INI: TDateTime;  /// Data inicial das informações contidas no arquivo
      FDT_FIN: TDateTime;  /// Data final das informações contidas no arquivo
      FGravado: Boolean;
    public
      property DT_INI : TDateTime read FDT_INI  write FDT_INI;
      property DT_FIN : TDateTime read FDT_FIN  write FDT_FIN;
      property Gravado: Boolean   read FGravado write FGravado ;
    end;

    {****************************************************************
     Conversões
     ****************************************************************}

    function QualifAssinanteToStr(const t: TSEFIIQualiAssinante): string;
    function StrToQualifAssinante(var ok: boolean; const s: string): TSEFIIQualiAssinante;
    function FinArquivoToStr(const t: TSEFIICodFinalidade): string;
    function StrToFinArquivo(var ok: boolean; const s: string): TSEFIICodFinalidade;
    function DocArquivoToStr(const t: TIndicadorDocArquivo): string;
    function StrToDocArquivo(var ok: boolean; const s: string): TIndicadorDocArquivo;
    function IndOperToStr(const t: TIndiceOperacao): string;
    function StrToIndOper(var ok: boolean; const s: string): TIndiceOperacao;
    function IndEmissaoToStr(const t: TIndiceEmissao): string;
    function StrToIndEmissao(var ok: boolean; const s: string): TIndiceEmissao;
    function CodSituacaoToStr(const t: TCodigoSituacao): string;
    function StrToCodSituacao(var ok: boolean; const s: string): TCodigoSituacao;
    function IndPagamentoToStr(const t: TIndicePagamento): string;
    function StrToIndPagamento(var ok: boolean; const s: string): TIndicePagamento;
    function ModDocumentoToStr(const t: TSEFIIDocFiscalReferenciado): string;
    function StrToModDocumento(var ok: boolean; const s: string): TSEFIIDocFiscalReferenciado;
    function IndExigEscrImpostoToStr(const t: TIndicadorExigeEscrImposto): string;
    function StrToIndExigEscrImposto(var ok: boolean; const s: string): TIndicadorExigeEscrImposto;
    function IndExigDiversaToStr(const t: TIndicadorExigeDiversas): string;
    function StrToIndExigDiversa(var ok: boolean; const s: string): TIndicadorExigeDiversas;
    function IndEscrContabilToStr(const t: TIndicadorEscrContabil): string;
    function StrToIndEscrContabil(var ok: boolean; const s: string): TIndicadorEscrContabil;
    function IndConteudoToStr(const t: TSEFIIIndicadorConteudo): string;
    function StrToBenefIndConteudo(var ok: boolean; const s: string): TSEFIIIndicadorConteudo;
    function IndContDocumentoToStr(const t: TSEFIIIndConteudoDocumento): string;
    function StrToIndContDocumento(var ok: boolean; const s: string): TSEFIIIndConteudoDocumento;
    function BenefFiscalICMSToStr(const t: TSEFIIBeniFiscalICMS): string;
    function StrToBenefFiscalICMS(var ok: boolean; const s: string): TSEFIIBeniFiscalICMS;
    function IndEntrDadosToStr(const t: TIndicadorDados): string;
    function StrToIndEntrDados(var ok: boolean; const s: string): TIndicadorDados;

    function IndExigRIDFToStr(const t : TSEFIIIndicadorExigRegImpDocFiscais) : string;
    function StrToExigRIDF(var ok: Boolean; const s: string): TSEFIIIndicadorExigRegImpDocFiscais;


    function CFOPToCOP(ACFOP : integer) : string;

implementation

function QualifAssinanteToStr(const t: TSEFIIQualiAssinante): string;
begin
  result := EnumeradoToStr(t, ['203','204','205','206','207','220','222','223','226','309','312','313','315','801','900','999'],
                              [qaDiretor,qaConsAdministracao,qaAdministrador,qaAdmGrupo,qaAdmSociedadeFiliada,qaAdmJudicialPessoaFisica,
                               qaAdmJudicialPessoaJuridica,qaAdmJudicial,qaGestorJudicial,qaProcurador,qaInventariante,qaLiquidante,
                               qaInterventor,qaEmpresario,qaContador,qaOutros]);
end;

function StrToQualifAssinante(var ok: boolean; const s: string): TSEFIIQualiAssinante;
begin
  result := StrToEnumerado(ok, s, ['203','204','205','206','207','220','222','223','226','309','312','313','315','801','900','999'],
                                  [qaDiretor,qaConsAdministracao,qaAdministrador,qaAdmGrupo,qaAdmSociedadeFiliada,qaAdmJudicialPessoaFisica,
                                   qaAdmJudicialPessoaJuridica,qaAdmJudicial,qaGestorJudicial,qaProcurador,qaInventariante,qaLiquidante,
                                   qaInterventor,qaEmpresario,qaContador,qaOutros]);
end;

function FinArquivoToStr(const t: TSEFIICodFinalidade): string;
begin
  result := EnumeradoToStr(t, ['0', '1'],
                              [raOriginal, raSubstituto]);
end;

function StrToFinArquivo(var ok: boolean; const s: string): TSEFIICodFinalidade;
begin
  result := StrToEnumerado(ok, s, ['0', '1'],
                              [raOriginal, raSubstituto]);
end;

function DocArquivoToStr(const t: TIndicadorDocArquivo): string;
begin
  result := EnumeradoToStr(t, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'],
                              [arqOriginal, arqTranscricaoEmissaoPropria, arqTranscricaoEmissaoTerceiros, arqTrancricaoDigitalizacao,
                               arqTranscricaoEmissaoEquipEspecilizado, arqLivrosResultadosObrigacoes, arqLivroMapaControles,
                               arqGuiasInfEconomicasFiscais, arqLivrosContabilidade, arqExtratoDocumentos]);
end;

function StrToDocArquivo(var ok: boolean; const s: string): TIndicadorDocArquivo;
begin
  result := StrToEnumerado(ok, s, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'],
                                  [arqOriginal, arqTranscricaoEmissaoPropria, arqTranscricaoEmissaoTerceiros, arqTrancricaoDigitalizacao,
                                   arqTranscricaoEmissaoEquipEspecilizado, arqLivrosResultadosObrigacoes, arqLivroMapaControles,
                                   arqGuiasInfEconomicasFiscais, arqLivrosContabilidade, arqExtratoDocumentos]);
end;

function IndOperToStr(const t: TIndiceOperacao): string;
begin
  result := EnumeradoToStr(t, ['0', '1'],
                              [SefioEntrada, SefioSaida]);
end;

function StrToIndOper(var ok: boolean; const s: string): TIndiceOperacao;
begin
  result := StrToEnumerado(ok, s, ['0', '1'],
                              [SefioEntrada, SefioSaida]);
end;

function IndEmissaoToStr(const t: TIndiceEmissao): string;
begin
  result := EnumeradoToStr(t, ['0', '1'],
                              [SefiePropria, SefieTerceiros]);
end;

function StrToIndEmissao(var ok: boolean; const s: string): TIndiceEmissao;
begin
  result := StrToEnumerado(ok, s, ['0', '1'],
                              [SefiePropria, SefieTerceiros]);
end;

function CodSituacaoToStr(const t: TCodigoSituacao): string;
begin
  result := EnumeradoToStr(t, ['0', '1', '2','3','4','5','6','7','10','20','25','80','81','90','91','95','99'],
                              [SefcsEmissaonormal, SefcsEmissaocontingencia, SefcsEmissaocontingenciaFS, SefcsEmissaocontingenciaSCAN,
                               SefcsEmissaocontingenciaDPEC, SefcsEmissaocontingenciaFSDA, SefcsEmissaoContingenciaSVCAN, SefcsEmissaoContingenciaSVCRS,
                               SefcsEmissaoavulsa, SefcsComplemento, SefcsConsolidavalores, SefcsAutorizadenegada, SefcsNumerainutilizada,
                               SefcsOperacancelada, SefcsNegociodesfeito, SefcsAjusteinformacoes, SefcsSemrepercussaofiscal]);
end;

function StrToCodSituacao(var ok: boolean; const s: string): TCodigoSituacao;
begin
  result := StrToEnumerado(ok, s, ['0', '1', '2','3','4','5','6','7','10','20','25','80','81','90','91','95','99'],
                              [SefcsEmissaonormal, SefcsEmissaocontingencia, SefcsEmissaocontingenciaFS, SefcsEmissaocontingenciaSCAN,
                               SefcsEmissaocontingenciaDPEC, SefcsEmissaocontingenciaFSDA, SefcsEmissaoContingenciaSVCAN, SefcsEmissaoContingenciaSVCRS,
                               SefcsEmissaoavulsa, SefcsComplemento, SefcsConsolidavalores, SefcsAutorizadenegada, SefcsNumerainutilizada,
                               SefcsOperacancelada, SefcsNegociodesfeito, SefcsAjusteinformacoes, SefcsSemrepercussaofiscal]);
end;

function IndPagamentoToStr(const t: TIndicePagamento): string;
begin
  result := EnumeradoToStr(t, ['0', '1', '2'],
                              [SefipAVista, SefAPrazo, SefNaoOnerada]);
end;

function StrToIndPagamento(var ok: boolean; const s: string): TIndicePagamento;
begin
  result := StrToEnumerado(ok, s, ['0', '1', '2'],
                              [SefipAVista, SefAPrazo, SefNaoOnerada]);
end;

function ModDocumentoToStr(const t: TSEFIIDocFiscalReferenciado): string;
begin
  result := EnumeradoToStr(t, ['01','02','2D','2E','04','06','07','08','09','10','11','13','14','15','16','17',
                               '18','20','21','22','23','24','25','26','27','28','29','30','31','55','57','65','98','99'],
                              [SrefNF,SrefNFVCCVC,SrefCCF,SrefCBP,SrefNFPR,SrefNFEE,SrefNFTR,SrefCTRC,SrefCTAQ,SrefCTAR,
                               SrefCTFC,SrefBPR,SrefBPAQ,SrefBPNB,SrefBPF,SrefDT,SrefRMD,SrefOCC,SrefNFSC,SrefNFST,SrefGNRE,
                               SrefACT,SrefMC,SrefCTMC,SrefNFTF,SrefNFGC,SrefNFAC,SrefMV,SrefBRP,SrefNFe,SrefCTe,SrefNFCe,Sref98,Sref99]);
end;

function StrToModDocumento(var ok: boolean; const s: string): TSEFIIDocFiscalReferenciado;
begin
  result := StrToEnumerado(ok, s, ['01','02','2D','2E','04','06','07','08','09','10','11','13','14','15','16','17',
                                   '18','20','21','22','23','24','25','26','27','28','29','30','31','55','57','65','98','99'],
                                  [SrefNF,SrefNFVCCVC,SrefCCF,SrefCBP,SrefNFPR,SrefNFEE,SrefNFTR,SrefCTRC,SrefCTAQ,SrefCTAR,
                                   SrefCTFC,SrefBPR,SrefBPAQ,SrefBPNB,SrefBPF,SrefDT,SrefRMD,SrefOCC,SrefNFSC,SrefNFST,SrefGNRE,
                                   SrefACT,SrefMC,SrefCTMC,SrefNFTF,SrefNFGC,SrefNFAC,SrefMV,SrefBRP,SrefNFe,SrefCTe,SrefNFCe,Sref98,Sref99]);
end;

function IndExigEscrImpostoToStr(const t: TIndicadorExigeEscrImposto): string;
begin
  result := EnumeradoToStr(t, ['0','1','2','9'],
                              [impSimRegimeSimplificado, impSimRegimeIntermediario, impSimRegimeIntegral, impNaoObrigado]);
end;


function StrToIndExigEscrImposto(var ok: boolean; const s: string): TIndicadorExigeEscrImposto;
begin
  result := StrToEnumerado(ok, s, ['0','1','2','9'],
                                  [impSimRegimeSimplificado, impSimRegimeIntermediario, impSimRegimeIntegral, impNaoObrigado]);
end;

function IndExigDiversaToStr(const t: TIndicadorExigeDiversas): string;
begin
  result := EnumeradoToStr(t, ['0','1',''],
                              [exSim,exNao,exVazio]);
end;

function StrToIndExigDiversa(var ok: boolean; const s: string): TIndicadorExigeDiversas;
begin
  result := StrToEnumerado(ok, s, ['0','1',''],
                                  [exSim,exNao,exVazio]);
end;

function IndEscrContabilToStr(const t: TIndicadorEscrContabil): string;
begin
  result := EnumeradoToStr(t, ['0','1','2','3','4','5','9',''],
                              [esCompletaArquivo, esCompletaPapel, esSimplificadaArquivo, esSimplificadaPapel, esLivroCaixaArquivo,
                               esLivroCaixaPapel, esNaoObrigado, esVazio]);
end;

function StrToIndEscrContabil(var ok: boolean; const s: string): TIndicadorEscrContabil;
begin
  result := StrToEnumerado(ok, s, ['0','1','2','3','4','5','9',''],
                                  [esCompletaArquivo, esCompletaPapel, esSimplificadaArquivo, esSimplificadaPapel, esLivroCaixaArquivo,
                                   esLivroCaixaPapel, esNaoObrigado  , esVazio]);
end;

function IndConteudoToStr(const t: TSEFIIIndicadorConteudo): string;
begin
  result := EnumeradoToStr(t, ['0','1'],
                              [icContConteudo, icSemConteudo]);
end;

function StrToBenefIndConteudo(var ok: boolean; const s: string): TSEFIIIndicadorConteudo;
begin
  result := StrToEnumerado(ok, s, ['0','1'],
                                  [icContConteudo, icSemConteudo]);
end;


function IndContDocumentoToStr(const t: TSEFIIIndConteudoDocumento): string;
begin
  result := EnumeradoToStr(t, ['0','1','2','3','4'],
                              [idDocEntrSaiAjuste, idDocSemLancamento, idDocEntrAjuste, idDocSaiAjuste, idDocLancAjuste]);
end;

function StrToIndContDocumento(var ok: boolean; const s: string): TSEFIIIndConteudoDocumento;
begin
  result := StrToEnumerado(ok, s, ['0','1','2','3','4'],
                                  [idDocEntrSaiAjuste, idDocSemLancamento, idDocEntrAjuste, idDocSaiAjuste, idDocLancAjuste]);
end;

function BenefFiscalICMSToStr(const t: TSEFIIBeniFiscalICMS): string;
begin
  result := EnumeradoToStr(t, ['','PE001'],
                              [bfNenhum, bfProdepe]);
end;

function StrToBenefFiscalICMS(var ok: boolean; const s: string): TSEFIIBeniFiscalICMS;
begin
  result := StrToEnumerado(ok, s, ['','PE001'],
                                  [bfNenhum, bfProdepe]);
end;

function IndEntrDadosToStr(const t: TIndicadorDados): string;
begin
  result := EnumeradoToStr(t, ['0','1','2','3'],
                              [iedDigitacaoDados, iedImportacaoArquivo, iedValidacaoArqTexto, iedExportacao]);

end;

function StrToIndEntrDados(var ok: boolean; const s: string): TIndicadorDados;
begin
  result := StrToEnumerado(ok, s, ['0','1','2','3'],
                                  [iedDigitacaoDados, iedImportacaoArquivo, iedValidacaoArqTexto, iedExportacao]);
end;

function IndExigRIDFToStr(const t : TSEFIIIndicadorExigRegImpDocFiscais) : string;
begin
  result := EnumeradoToStr(t, ['0','1',''],
                              [idSim, idNao, idVazio]);
end;

function StrToExigRIDF(var ok: Boolean; const s: string): TSEFIIIndicadorExigRegImpDocFiscais;
begin
  result := StrToEnumerado(ok, s, ['0','1',''],
                                  [idSim, idNao, idVazio]);
end;

function CFOPToCOP(ACFOP : integer) : string;
begin
  Case ACFOP of
    0    : Result := 'OP00';
    1101 : Result := 'EA10';
    1102 : Result := 'EA10';
    1111 : Result := 'EA10';
    1113 : Result := 'EA10';
    1116 : Result := 'EA10';
    1117 : Result := 'EA10';
    1118 : Result := 'EA10';
    1120 : Result := 'EA10';
    1121 : Result := 'EA10';
    1122 : Result := 'EA10';
    1124 : Result := 'EA30';
    1125 : Result := 'EA30';
    1126 : Result := 'EA10';
    1128 : Result := 'EA10';
    1151 : Result := 'EA60';
    1152 : Result := 'EA60';
    1153 : Result := 'EA60';
    1154 : Result := 'EA60';
    1201 : Result := 'EA20';
    1202 : Result := 'EA20';
    1203 : Result := 'EA20';
    1204 : Result := 'EA20';
    1205 : Result := 'EA80';
    1206 : Result := 'EA80';
    1207 : Result := 'EA80';
    1208 : Result := 'EA60';
    1209 : Result := 'EA60';
    1251 : Result := 'EA10';
    1252 : Result := 'EA10';
    1253 : Result := 'EA10';
    1254 : Result := 'EA10';
    1255 : Result := 'EA10';
    1256 : Result := 'EA10';
    1257 : Result := 'EA10';
    1301 : Result := 'EA70';
    1302 : Result := 'EA70';
    1303 : Result := 'EA70';
    1304 : Result := 'EA70';
    1305 : Result := 'EA70';
    1306 : Result := 'EA70';
    1351 : Result := 'EA70';
    1352 : Result := 'EA70';
    1353 : Result := 'EA70';
    1354 : Result := 'EA70';
    1355 : Result := 'EA70';
    1356 : Result := 'EA70';
    1360 : Result := 'EA70';
    1401 : Result := 'EA10';
    1403 : Result := 'EA10';
    1406 : Result := 'EA10';
    1407 : Result := 'EA10';
    1408 : Result := 'EA60';
    1409 : Result := 'EA60';
    1410 : Result := 'EA20';
    1411 : Result := 'EA20';
    1414 : Result := 'EA30';
    1415 : Result := 'EA30';
    1451 : Result := 'EA30';
    1452 : Result := 'EA30';
    1501 : Result := 'EA50';
    1503 : Result := 'EA40';
    1504 : Result := 'EA40';
    1505 : Result := 'EA40';
    1506 : Result := 'EA40';
    1551 : Result := 'EA10';
    1552 : Result := 'EA60';
    1553 : Result := 'EA20';
    1554 : Result := 'EA30';
    1555 : Result := 'EA50';
    1556 : Result := 'EA10';
    1557 : Result := 'EA60';
    1601 : Result := 'EA90';
    1602 : Result := 'EA90';
    1603 : Result := 'EA91';
    1604 : Result := 'EA90';
    1605 : Result := 'EA90';
    1651 : Result := 'EA10';
    1652 : Result := 'EA10';
    1653 : Result := 'EA10';
    1658 : Result := 'EA60';
    1659 : Result := 'EA60';
    1660 : Result := 'EA20';
    1661 : Result := 'EA20';
    1662 : Result := 'EA20';
    1663 : Result := 'EA50';
    1664 : Result := 'EA30';
    1901 : Result := 'EA50';
    1902 : Result := 'EA30';
    1903 : Result := 'EA30';
    1904 : Result := 'EA30';
    1905 : Result := 'EA50';
    1906 : Result := 'EA30';
    1907 : Result := 'EA30';
    1908 : Result := 'EA50';
    1909 : Result := 'EA30';
    1910 : Result := 'EA50';
    1911 : Result := 'EA50';
    1912 : Result := 'EA50';
    1913 : Result := 'EA30';
    1914 : Result := 'EA30';
    1915 : Result := 'EA50';
    1916 : Result := 'EA30';
    1917 : Result := 'EA50';
    1918 : Result := 'EA30';
    1919 : Result := 'EA30';
    1920 : Result := 'EA50';
    1921 : Result := 'EA30';
    1922 : Result := 'EA90';
    1923 : Result := 'EA50';
    1924 : Result := 'EA50';
    1925 : Result := 'EA30';
    1926 : Result := 'EA65';
    1931 : Result := 'EA90';
    1932 : Result := 'EA70';
    1933 : Result := 'EA10';
    1934 : Result := 'EA50';
    1949 : Result := 'EA99';
    2101 : Result := 'EA10';
    2102 : Result := 'EA10';
    2111 : Result := 'EA10';
    2113 : Result := 'EA10';
    2116 : Result := 'EA10';
    2117 : Result := 'EA10';
    2118 : Result := 'EA10';
    2120 : Result := 'EA10';
    2121 : Result := 'EA10';
    2122 : Result := 'EA10';
    2124 : Result := 'EA30';
    2125 : Result := 'EA30';
    2126 : Result := 'EA10';
    2128 : Result := 'EA10';
    2151 : Result := 'EA60';
    2152 : Result := 'EA60';
    2153 : Result := 'EA60';
    2154 : Result := 'EA60';
    2201 : Result := 'EA20';
    2202 : Result := 'EA20';
    2203 : Result := 'EA20';
    2204 : Result := 'EA20';
    2205 : Result := 'EA80';
    2206 : Result := 'EA80';
    2207 : Result := 'EA80';
    2208 : Result := 'EA60';
    2209 : Result := 'EA60';
    2251 : Result := 'EA10';
    2252 : Result := 'EA10';
    2253 : Result := 'EA10';
    2254 : Result := 'EA10';
    2255 : Result := 'EA10';
    2256 : Result := 'EA10';
    2257 : Result := 'EA10';
    2301 : Result := 'EA70';
    2302 : Result := 'EA70';
    2303 : Result := 'EA70';
    2304 : Result := 'EA70';
    2305 : Result := 'EA70';
    2306 : Result := 'EA70';
    2351 : Result := 'EA70';
    2352 : Result := 'EA70';
    2353 : Result := 'EA70';
    2354 : Result := 'EA70';
    2355 : Result := 'EA70';
    2356 : Result := 'EA70';
    2401 : Result := 'EA10';
    2403 : Result := 'EA10';
    2406 : Result := 'EA10';
    2407 : Result := 'EA10';
    2408 : Result := 'EA60';
    2409 : Result := 'EA60';
    2410 : Result := 'EA20';
    2411 : Result := 'EA20';
    2414 : Result := 'EA30';
    2415 : Result := 'EA30';
    2501 : Result := 'EA50';
    2503 : Result := 'EA40';
    2504 : Result := 'EA40';
    2505 : Result := 'EA40';
    2506 : Result := 'EA40';
    2551 : Result := 'EA10';
    2552 : Result := 'EA60';
    2553 : Result := 'EA20';
    2554 : Result := 'EA30';
    2555 : Result := 'EA50';
    2556 : Result := 'EA10';
    2557 : Result := 'EA60';
    2603 : Result := 'EA91';
    2651 : Result := 'EA10';
    2652 : Result := 'EA10';
    2653 : Result := 'EA10';
    2658 : Result := 'EA60';
    2659 : Result := 'EA60';
    2660 : Result := 'EA20';
    2661 : Result := 'EA20';
    2662 : Result := 'EA20';
    2663 : Result := 'EA50';
    2664 : Result := 'EA30';
    2901 : Result := 'EA50';
    2902 : Result := 'EA30';
    2903 : Result := 'EA30';
    2904 : Result := 'EA30';
    2905 : Result := 'EA50';
    2906 : Result := 'EA30';
    2907 : Result := 'EA30';
    2908 : Result := 'EA50';
    2909 : Result := 'EA30';
    2910 : Result := 'EA50';
    2911 : Result := 'EA50';
    2912 : Result := 'EA50';
    2913 : Result := 'EA30';
    2914 : Result := 'EA30';
    2915 : Result := 'EA50';
    2916 : Result := 'EA30';
    2917 : Result := 'EA50';
    2918 : Result := 'EA40';
    2919 : Result := 'EA30';
    2920 : Result := 'EA50';
    2921 : Result := 'EA30';
    2922 : Result := 'EA90';
    2923 : Result := 'EA50';
    2924 : Result := 'EA50';
    2925 : Result := 'EA30';
    2931 : Result := 'EA90';
    2932 : Result := 'EA70';
    2933 : Result := 'EA10';
    2934 : Result := 'EA50';
    2949 : Result := 'EA99';
    3101 : Result := 'EA10';
    3102 : Result := 'EA10';
    3126 : Result := 'EA10';
    3127 : Result := 'EA10';
    3128 : Result := 'EA10';
    3201 : Result := 'EA20';
    3202 : Result := 'EA20';
    3205 : Result := 'EA80';
    3206 : Result := 'EA80';
    3207 : Result := 'EA80';
    3211 : Result := 'EA20';
    3251 : Result := 'EA10';
    3301 : Result := 'EA70';
    3351 : Result := 'EA70';
    3352 : Result := 'EA70';
    3353 : Result := 'EA70';
    3354 : Result := 'EA70';
    3355 : Result := 'EA70';
    3356 : Result := 'EA70';
    3503 : Result := 'EA40';
    3551 : Result := 'EA10';
    3553 : Result := 'EA20';
    3556 : Result := 'EA10';
    3651 : Result := 'EA10';
    3652 : Result := 'EA10';
    3653 : Result := 'EA10';
    3930 : Result := 'EA50';
    3949 : Result := 'EA99';
    5101 : Result := 'SP10';
    5102 : Result := 'SP10';
    5103 : Result := 'SP10';
    5104 : Result := 'SP10';
    5105 : Result := 'SP10';
    5106 : Result := 'SP10';
    5109 : Result := 'SP10';
    5110 : Result := 'SP10';
    5111 : Result := 'SP10';
    5112 : Result := 'SP10';
    5113 : Result := 'SP10';
    5114 : Result := 'SP10';
    5115 : Result := 'SP10';
    5116 : Result := 'SP10';
    5117 : Result := 'SP10';
    5118 : Result := 'SP10';
    5119 : Result := 'SP10';
    5120 : Result := 'SP10';
    5122 : Result := 'SP10';
    5123 : Result := 'SP10';
    5124 : Result := 'SP50';
    5125 : Result := 'SP50';
    5151 : Result := 'SP60';
    5152 : Result := 'SP60';
    5153 : Result := 'SP60';
    5155 : Result := 'SP60';
    5156 : Result := 'SP60';
    5201 : Result := 'SP20';
    5202 : Result := 'SP20';
    5205 : Result := 'SP80';
    5206 : Result := 'SP80';
    5207 : Result := 'SP80';
    5208 : Result := 'SP60';
    5209 : Result := 'SP60';
    5210 : Result := 'SP20';
    5251 : Result := 'SP10';
    5252 : Result := 'SP10';
    5253 : Result := 'SP10';
    5254 : Result := 'SP10';
    5255 : Result := 'SP10';
    5256 : Result := 'SP10';
    5257 : Result := 'SP10';
    5258 : Result := 'SP10';
    5301 : Result := 'SP70';
    5302 : Result := 'SP70';
    5303 : Result := 'SP70';
    5304 : Result := 'SP70';
    5305 : Result := 'SP70';
    5306 : Result := 'SP70';
    5307 : Result := 'SP70';
    5351 : Result := 'SP70';
    5352 : Result := 'SP70';
    5353 : Result := 'SP70';
    5354 : Result := 'SP70';
    5355 : Result := 'SP70';
    5356 : Result := 'SP70';
    5357 : Result := 'SP70';
    5359 : Result := 'SP70';
    5360 : Result := 'SP70';
    5401 : Result := 'SP10';
    5402 : Result := 'SP10';
    5403 : Result := 'SP10';
    5405 : Result := 'SP10';
    5408 : Result := 'SP60';
    5409 : Result := 'SP60';
    5410 : Result := 'SP20';
    5411 : Result := 'SP20';
    5412 : Result := 'SP20';
    5413 : Result := 'SP20';
    5414 : Result := 'SP30';
    5415 : Result := 'SP30';
    5451 : Result := 'SP30';
    5501 : Result := 'SP30';
    5502 : Result := 'SP30';
    5503 : Result := 'SP50';
    5504 : Result := 'SP30';
    5505 : Result := 'SP30';
    5551 : Result := 'SP10';
    5552 : Result := 'SP60';
    5553 : Result := 'SP20';
    5554 : Result := 'SP30';
    5555 : Result := 'SP50';
    5556 : Result := 'SP20';
    5557 : Result := 'SP60';
    5601 : Result := 'SP90';
    5602 : Result := 'SP90';
    5603 : Result := 'SP91';
    5605 : Result := 'SP90';
    5606 : Result := 'SP90';
    5651 : Result := 'SP10';
    5652 : Result := 'SP10';
    5653 : Result := 'SP10';
    5654 : Result := 'SP10';
    5655 : Result := 'SP10';
    5656 : Result := 'SP10';
    5657 : Result := 'SP30';
    5658 : Result := 'SP60';
    5659 : Result := 'SP60';
    5660 : Result := 'SP20';
    5661 : Result := 'SP20';
    5662 : Result := 'SP20';
    5663 : Result := 'SP30';
    5664 : Result := 'SP50';
    5665 : Result := 'SP50';
    5666 : Result := 'SP30';
    5667 : Result := 'SP10';
    5901 : Result := 'SP30';
    5902 : Result := 'SP50';
    5903 : Result := 'SP50';
    5904 : Result := 'SP30';
    5905 : Result := 'SP30';
    5906 : Result := 'SP50';
    5907 : Result := 'SP50';
    5908 : Result := 'SP30';
    5909 : Result := 'SP50';
    5910 : Result := 'SP30';
    5911 : Result := 'SP30';
    5912 : Result := 'SP30';
    5913 : Result := 'SP50';
    5914 : Result := 'SP30';
    5915 : Result := 'SP30';
    5916 : Result := 'SP50';
    5917 : Result := 'SP30';
    5918 : Result := 'SP50';
    5919 : Result := 'SP50';
    5920 : Result := 'SP30';
    5921 : Result := 'SP50';
    5922 : Result := 'SP90';
    5923 : Result := 'SP30';
    5924 : Result := 'SP30';
    5925 : Result := 'SP50';
    5926 : Result := 'SP65';
    5927 : Result := 'SP65';
    5928 : Result := 'SP65';
    5929 : Result := 'SP90';
    5931 : Result := 'SP90';
    5932 : Result := 'SP70';
    5933 : Result := 'SP10';
    5934 : Result := 'SP30';
    5949 : Result := 'SP99';
    6101 : Result := 'SP10';
    6102 : Result := 'SP10';
    6103 : Result := 'SP10';
    6104 : Result := 'SP10';
    6105 : Result := 'SP10';
    6106 : Result := 'SP10';
    6107 : Result := 'SP10';
    6108 : Result := 'SP10';
    6109 : Result := 'SP10';
    6110 : Result := 'SP10';
    6111 : Result := 'SP10';
    6112 : Result := 'SP10';
    6113 : Result := 'SP10';
    6114 : Result := 'SP10';
    6115 : Result := 'SP10';
    6116 : Result := 'SP10';
    6117 : Result := 'SP10';
    6118 : Result := 'SP10';
    6119 : Result := 'SP10';
    6120 : Result := 'SP10';
    6122 : Result := 'SP10';
    6123 : Result := 'SP10';
    6124 : Result := 'SP50';
    6125 : Result := 'SP50';
    6151 : Result := 'SP60';
    6152 : Result := 'SP60';
    6153 : Result := 'SP60';
    6155 : Result := 'SP60';
    6156 : Result := 'SP60';
    6201 : Result := 'SP20';
    6202 : Result := 'SP20';
    6205 : Result := 'SP80';
    6206 : Result := 'SP80';
    6207 : Result := 'SP80';
    6208 : Result := 'SP50';
    6209 : Result := 'SP50';
    6210 : Result := 'SP20';
    6251 : Result := 'SP10';
    6252 : Result := 'SP10';
    6253 : Result := 'SP10';
    6254 : Result := 'SP10';
    6255 : Result := 'SP10';
    6256 : Result := 'SP10';
    6257 : Result := 'SP10';
    6258 : Result := 'SP10';
    6301 : Result := 'SP70';
    6302 : Result := 'SP70';
    6303 : Result := 'SP70';
    6304 : Result := 'SP70';
    6305 : Result := 'SP70';
    6306 : Result := 'SP70';
    6307 : Result := 'SP70';
    6351 : Result := 'SP70';
    6352 : Result := 'SP70';
    6353 : Result := 'SP70';
    6354 : Result := 'SP70';
    6355 : Result := 'SP70';
    6356 : Result := 'SP70';
    6357 : Result := 'SP70';
    6359 : Result := 'SP70';
    6360 : Result := 'SP70';
    6401 : Result := 'SP10';
    6402 : Result := 'SP10';
    6403 : Result := 'SP10';
    6404 : Result := 'SP10';
    6408 : Result := 'SP60';
    6409 : Result := 'SP60';
    6410 : Result := 'SP20';
    6411 : Result := 'SP20';
    6412 : Result := 'SP20';
    6413 : Result := 'SP20';
    6414 : Result := 'SP30';
    6415 : Result := 'SP30';
    6501 : Result := 'SP30';
    6502 : Result := 'SP30';
    6503 : Result := 'SP50';
    6504 : Result := 'SP30';
    6505 : Result := 'SP30';
    6551 : Result := 'SP10';
    6552 : Result := 'SP60';
    6553 : Result := 'SP20';
    6554 : Result := 'SP30';
    6555 : Result := 'SP50';
    6556 : Result := 'SP20';
    6557 : Result := 'SP60';
    6603 : Result := 'SP91';
    6651 : Result := 'SP10';
    6652 : Result := 'SP10';
    6653 : Result := 'SP10';
    6654 : Result := 'SP10';
    6655 : Result := 'SP10';
    6656 : Result := 'SP10';
    6657 : Result := 'SP30';
    6658 : Result := 'SP60';
    6659 : Result := 'SP60';
    6660 : Result := 'SP20';
    6661 : Result := 'SP20';
    6662 : Result := 'SP20';
    6663 : Result := 'SP30';
    6664 : Result := 'SP50';
    6665 : Result := 'SP50';
    6666 : Result := 'SP30';
    6667 : Result := 'SP10';
    6901 : Result := 'SP30';
    6902 : Result := 'SP50';
    6903 : Result := 'SP50';
    6904 : Result := 'SP30';
    6905 : Result := 'SP30';
    6906 : Result := 'SP50';
    6907 : Result := 'SP50';
    6908 : Result := 'SP30';
    6909 : Result := 'SP50';
    6910 : Result := 'SP30';
    6911 : Result := 'SP30';
    6912 : Result := 'SP30';
    6913 : Result := 'SP50';
    6914 : Result := 'SP30';
    6915 : Result := 'SP30';
    6916 : Result := 'SP50';
    6917 : Result := 'SP30';
    6918 : Result := 'SP50';
    6919 : Result := 'SP50';
    6920 : Result := 'SP30';
    6921 : Result := 'SP50';
    6922 : Result := 'SP90';
    6923 : Result := 'SP30';
    6924 : Result := 'SP30';
    6925 : Result := 'SP50';
    6929 : Result := 'SP90';
    6931 : Result := 'SP90';
    6932 : Result := 'SP70';
    6933 : Result := 'SP10';
    6934 : Result := 'SP30';
    6949 : Result := 'SP99';
    7101 : Result := 'SP10';
    7102 : Result := 'SP10';
    7105 : Result := 'SP10';
    7106 : Result := 'SP10';
    7127 : Result := 'SP10';
    7201 : Result := 'SP20';
    7202 : Result := 'SP20';
    7205 : Result := 'SP80';
    7206 : Result := 'SP80';
    7207 : Result := 'SP80';
    7210 : Result := 'SP20';
    7211 : Result := 'SP20';
    7251 : Result := 'SP10';
    7301 : Result := 'SP70';
    7358 : Result := 'SP70';
    7501 : Result := 'SP10';
    7551 : Result := 'SP10';
    7553 : Result := 'SP20';
    7556 : Result := 'SP20';
    7651 : Result := 'SP10';
    7654 : Result := 'SP10';
    7667 : Result := 'SP10';
    7930 : Result := 'SP20';
    7949 : Result := 'SP99';
  end;
end;

{ TACBrSEFIIRegistros }

function TACBrSEFIIRegistros.AchaUltimoPai(ANomePai,
  ANomeFilho: String): Integer;
begin
   Result := Count - 1;
   if Result < 0 then
     raise Exception.CreateFmt('O registro %:0s deve ser filho do registro %:1s, ' +
                               'e não existe nenhum %:1s pai!', [ANomeFilho, ANomePai]);
end;

end.
