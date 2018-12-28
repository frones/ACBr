{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2015   João Pedro R Costa                   }
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
{ Hw Sistemas e Computadores Ltda -  contato@hwsistemas.com.br                 }
{ www.hwsistemas.com.br - joaopedro@hwsistemas.com.br                          }
{              Rua Mogno, 236 - Governado Valadares - MG - 35065-019           }
{                                                                              }
{******************************************************************************}

unit ACBrDeSTDABlocos;

interface

uses
  SysUtils, Classes, DateUtils, ACBrTXTClass, contnrs;

type
  /// Indicador de movimento - TOpenBlocos
  TACBrIndMov = (imComDados, // 0- Bloco com dados informados;
                 imSemDados  // 1- Bloco sem dados informados.
                             );
  TACBrIndicadorMovimento = TACBrIndMov;

  /// Tabela Versão do Leiaute - item 3.1.1 - TRegistro0000
  TACBrCodVer = (
    vlVersao2000,  // Código 2000 - Versão 2.0.0.0 Ato COTEPE 47 2015
    vlVersao2001,  //
    vlVersao2010,  //
    vlVersao2100
  );

  TACBrVersaoLeiaute = TACBrCodVer;

  // Tabela Finalidade do Arquivo - item 3.2.1 - TRegistro0000
  TACBrCodFin           = (raOriginal,     // 0 - Remessa do arquivo original
                           raSubstituto    // 1 - Remessa do arquivo substituto
                             );
  TACBrCodFinalidade = TACBrCodFin;

  // Tabela Conteúdo do Arquivo-texto - item 3.2.2 - TRegistro0000
  TACBrConteudoArquivo = (
    cn30 ,      // 30 -  Resumos e informações consolidadas
    cnNullo     //    - campo nulo
  );

  // Tabela Qualificação de Assinantes - item 3.3.2 - TRegistro0005
  TACBrAssinante =
  (
    asDiretor,                 // 203 - Diretor
    asConsAdm,                 // 204 - Conselheiro de administração
    asAdministrador,           // 205 - Administrador
    asAdmGrupo,                // 206 - Administrador de grupo
    asAdmSocFiliada,           // 207 - Administrador de sociedade filiada
    asAdmJudicialPF,           // 220 - Administrador judicial - pessoa física
    asAdmJudicialPJPR,         // 222 - Administrador judicial - pessoa jurídica/profissional responsável
    asAdmJudicialGestor,       // 223 - Administrador judicial - gestor
    asGestorJudicial,          // 226 - Gestor judicial
    asProcurador,              // 309 - Procurador
    asInventariante,           // 312 - Inventariante
    asLiquidante,              // 313 - Liquidante
    asInterventor,             // 315 - Interventor
    asEmpresario,              // 801 - Empresário
    asContador,                // 900 - Contador
    asOutros                   // 999 - Outros
  );

  // Indicador de entrada de dados - TRegistro 0030
  TACBrIndEntrada =
  (
    edDigitacao,           // 0 - Digitação de dados
    edImportacaoArquivo,   // 1 - Importação de arquivo-texto
    edAdicaoDoc_ArqTexto,  // 2 - Adição de documentos/arquivo-texto
    edExportArquivo        // 3 - Exportação de arquivo-texto
  );

  // Indicador do documento contido no arquivo - TRegistro 0030
  TACBrIndDocumentos = (
    dcGuiasInfEconFisc	//7- Guias de informações econômico-fiscais
  );

  // Indicador de exigibilidade da escrituração do ISS - TRegistro 0030
  TACBrIndExigISS = (
    exISSSimplificado, // 0 - Sim, no modo simplificado de escrituração do imposto
    exISSIntegral,     // 2 - Sim, no modo integral de escrituração do regime normal de apuração do imposto
    exISSNaoObrigado   // 9 - Não obrigado a escritura
  );

  // Indicador de exigibilidade da escrituração do ICMS - TRegistro 0030
  TACBrIndExigICMS = (
    exICMSSimplificado,  // 0 - Sim, no modo simplificado de escrituração do imposto
    exICMSIntermediario, // 1 - Sim, no modo intermediário de escrituração do regime normal de apuração do imposto
    exICMSIntegral,      // 2 - Sim, no modo integral de escrituração do regime normal de apuração do imposto
    exICMSNaoObrigado    // 9 - Não obrigado a escriturar
  );

  //TipoGenerico de Sim e Nao
  TACBrSimNao = (
    snSim, // 0 - Sim
    snNao  // 1 - Não
  );

  // Indicador de exigibilidade do Registro de Impressão de Documentos Fiscais - TRegistro 0030
  TACBrIndExigImpDocF = TACBrSimNao;

  // Indicador de exigibilidade do Registro de Utilização de Documentos Fiscais - TRegistro 0030
  TACBrIndExigUtilDocF = TACBrSimNao;

  // Indicador de exigibilidade do Livro de Movimentação de Combustíveis - TRegistro 0030
  TACBrIndExigMovComb = TACBrSimNao;

  // Indicador de exigibilidade do Registro de Veículos - TRegistro 0030
  TACBrIndExigRegVeic = TACBrSimNao;

  // ndicador de exigibilidade anual do Registro de Inventário - TRegistro 0030
  TACBrIndExigRegInv = TACBrSimNao;

  // Indicador de apresentação da escrituração contábil - TRegistro 0030
  TACBrIndApresEC =
  (
   ecCompletaEmArquivoDig,       // 1 - Completa registrada em arquivo digital
   ecCompletaRegistradaPapel,    // 2 - Completa registrada em papel, microfilme, fichas avulsas, ou 3 - fichas/folhas contínuas
   ecSimplificadaEmArquivoDig,   // 3 - Simplificada registrada em arquivo digital, Simplificada registrada papel, microfilme, fichas avulsas, ou fichas/folhas contínuas
   ecLivroCaixaArquivoDig,       // 4 - Livro Caixa registrado em arquivo digital
   ecLivroCaixaRegistradoPapel,  // 5 - Livro Caixa registrado papel, microfilme, fichas avulsas, ou fichas/folhas contínuas
   ecNaoObrigado                 // 9 - Não obrigado a escriturar
  );

  // Indicador de operações sujeitas ao ISS - TRegistro 0030
  TACBrIndOpISS = TACBrSimNao;

  // Indicador de operações sujeitas à retenção tributária do ISS, na condição de contribuinte-substituído - TRegistro 0030
  TACBrIndOpISSRet = TACBrSimNao;

  // Indicador de operações sujeitas ao ICMS - TRegistro 0030
  TACBrIndOpICMS = TACBrSimNao;

  // Indicador de operações sujeitas à substituição tributária do ICMS, na condição de contribuinte-substituto - TRegistro 0030
  TACBrIndOpICMSST = TACBrSimNao;

  // Indicador de operações sujeitas à antecipação tributária do ICMS, nas entradas - TRegistro 0030
  TACBrIndOpICMSAnt = TACBrSimNao;

  // Indicador de operações sujeitas ao IPI - TRegistro 0030
  TACBrIndOpIPI = TACBrSimNao;

  // Indicador de apresentação avulsa do Registro de Inventário - TRegistro 0030
  TACBrIndApresAvlRegInv = TACBrSimNao;

  //  Indicador de conteúdo - TRegistro G020
  TACBrIndContGuias =
  (
      igISS,         // 0 - Guia com informações de operações do ISS
      igICMS,        // 1 - Guia com informações de operações do ICMS
      igSimples,     // 2 - Guia com informações de operações do Simples Nacional
      igRecalcICMS,  // 8 - Resultado do recálculo do valor adicionado em operações do ICMS
      igGuiaSemDados // 9 - Guia sem dados informados
  );

  TACBrIndSitDifAnt =
  (
    dfAntEntAtivoPermanente,  // 0 - Diferencia de alíquota pelas entradas interestaduais para aquisição de ativo permanente
    dfAntEntUsoEConsumo,      // 1 - Diferencial de Alíquota pelas entradas interestaduais para aquisição de mercadoria para o uso e/ou consumo
    dfAntEntEncerrSemCobICMS, // 2 - Antecipação pelas entradas interestaduais com encerramento de fase de tributação (sem cobrança de ICMS nas operações subsequentes)
    dfAntEntEncerrComCobICMS  // 3 - Antecipação pelas entradas interestaduais sem encerramento de fase de tributação (com cobrança de ICMS nas operações subsequentes)
  );

  TACBrIndOper      = (tpEntradaAquisicao, // 0 - Entrada
                       tpSaidaPrestacao    // 1 - Saída
                       );
  TACBrTipoOperacao = TACBrIndOper;

  /// Indicador do emitente do documento fiscal
  TACBrIndEmit = (edEmissaoPropria,         // 0 - Emissão própria
                  edTerceiros               // 1 - Terceiro
                  );
  TACBrEmitente = TACBrIndEmit;

  TACBrIndTipoST =
  (
    stOpSubsequentes,      // 0 - ICMS ST Operações Subsequentes
    stOpAntecedentes,      // 1 - ICMS ST Operações Antecedentes
    stServicosTransportes, // 2 - ICMS ST Serviço de Transporte (operações concomitantes)
    stRefCombustiveis      // 3 - ICMS ST Ref. a Combustíveis
  );

  TACBrDeLeiauteArquivos =
  (
    laLFPD, /// Texto fixo "LFPD"
    laLECD  // Texto fixo "LECD"
  );

  TOpenBlocos = class
  private
    FIND_MOV: TACBrIndicadorMovimento;    /// Indicador de movimento: 0- Bloco com dados informados, 1- Bloco sem dados informados.
  public
    property IND_MOV: TACBrIndicadorMovimento read FIND_MOV write FIND_MOV;
  end;

  //Funcoes do ACBrDeSTDABlocos
  function strToCodAssinante( const AValue: string ): TACBrAssinante;
  function codAssinanteToStr( AValue: TACBrAssinante ): string;

implementation



function strToCodAssinante( const AValue: string ): TACBrAssinante;
begin
 if AValue = '203' then Result := asDiretor
 else if AValue = '204' then Result := asConsAdm
 else if AValue = '205' then Result := asAdministrador
 else if AValue = '206' then Result := asAdmGrupo
 else if AValue = '207' then Result := asAdmSocFiliada
 else if AValue = '220' then Result := asAdmJudicialPF
 else if AValue = '222' then Result := asAdmJudicialPJPR
 else if AValue = '223' then Result := asAdmJudicialGestor
 else if AValue = '226' then Result := asGestorJudicial
 else if AValue = '309' then Result := asProcurador
 else if AValue = '312' then Result := asInventariante
 else if AValue = '313' then Result := asLiquidante
 else if AValue = '315' then Result := asInterventor
 else if AValue = '801' then Result := asEmpresario
 else if AValue = '900' then Result := asContador
 else
   Result := asOutros;
end;

function codAssinanteToStr( AValue: TACBrAssinante ): string;
begin
  case AValue of
    asDiretor          : Result := '203';
    asConsAdm          : Result := '204';
    asAdministrador    : Result := '205';
    asAdmGrupo         : Result := '206';
    asAdmSocFiliada    : Result := '207';
    asAdmJudicialPF    : Result := '220';
    asAdmJudicialPJPR  : Result := '222';
    asAdmJudicialGestor: Result := '223';
    asGestorJudicial   : Result := '226';
    asProcurador       : Result := '309';
    asInventariante    : Result := '312';
    asLiquidante       : Result := '313';
    asInterventor      : Result := '315';
    asEmpresario       : Result := '801';
    asContador         : Result := '900';
    asOutros           : Result := '999';
  end;
end;

end.
