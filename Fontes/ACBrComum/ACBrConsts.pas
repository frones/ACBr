{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados(c)2007  João Carvalho - SIGData Soluções em TI  }
{                                                                              }
{ Colaboradores nesse arquivo: Daniel Simoes de Almeida                        }
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
{ http://www.opensource.org/licenses/gpl-license.php                           }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 09/06/2008:  João Carvalho - SIGData Soluções em TI
|* - Primeira Versao: Criaçao e Distribuiçao da Primeira Versao
******************************************************************************}

{$I ACBr.inc}

unit ACBrConsts;

interface

Uses
  {$IFNDEF COMPILER6_UP}
    ACBrD5,
  {$ENDIF}
   SysUtils;

// delphi XE3 em diante não possui mais essas var, então criar e preencher
{$IFDEF DELPHI15_UP}
var
  fmtst: TFormatSettings;
  CurrencyString: string;
  CurrencyFormat: Byte;
  NegCurrFormat: Byte;
  ThousandSeparator: Char;
  DecimalSeparator: Char;
  CurrencyDecimals: Byte;
  DateSeparator: Char;
  ShortDateFormat: string;
  LongDateFormat: string;
  TimeSeparator: Char;
  TimeAMString: string;
  TimePMString: string;
  ShortTimeFormat: string;
  LongTimeFormat: string;
  TwoDigitYearCenturyWindow: Word = 50;
  ListSeparator: Char;
{$ENDIF}

const
  {* Unit ACBrBase *}
  ACBR_VERSAO = '0.9.0a';
  NUL = #00 ;
  SOH = #01 ;
  STX = #02 ;
  ETX = #03 ;
  EOT = #04 ;
  ENQ = #05 ;
  ACK = #06 ;
  BELL= #07 ;
  BS  = #08 ;
  TAB = #09 ;
  LF  = #10 ;
  FF  = #12 ;
  CR  = #13 ;
  SO  = #14 ;
  SI  = #15 ;
  DLE = #16 ;
  WAK = #17 ;
  DC2 = #18 ;
  DC4 = #20 ;
  NAK = #21 ;
  SYN = #22 ;
  ESC = #27 ;
  FS  = #28 ;
  GS  = #29 ;
  CTRL_Z = #26 ;
  CRLF = CR + LF ;

  CUTF8CodPage = 65001;
  CUTF8BOM = #239+#187+#191;
  CUTF8DeclaracaoXML = '<?xml version="1.0" encoding="UTF-8"?>';

  cTimeout = 3 ;  { Tempo PADRAO para msg de falha de comunicacao }
  CDotsMM = 8;  // 203dpi

  cTagLigaExpandido         = '<e>';
  cTagDesligaExpandido      = '</e>';
  cTagLigaAlturaDupla       = '<a>';
  cTagDesligaAlturaDupla    = '</a>';
  cTagLigaNegrito           = '<n>';
  cTagDesligaNegrito        = '</n>';
  cTagLigaSublinhado        = '<s>';
  cTagDesligaSublinhado     = '</s>';
  cTagLigaCondensado        = '<c>';
  cTagDesligaCondensado     = '</c>';
  cTagLigaItalico           = '<i>';
  cTagDesligaItalico        = '</i>';
  cTagLigaInvertido         = '<in>';
  cTagDesligaInvertido      = '</in>';
  cTagFonteNormal           = '</fn>';
  cTagFonteA                = '</fa>';
  cTagFonteB                = '</fb>';
  cTagFonteAlinhadaDireita  = '</ad>';
  cTagFonteAlinhadaEsquerda = '</ae>';
  cTagfonteAlinhadaCentro   = '</ce>';

  cTAGS_CARACTER: array[0..12] of String = (
    cTagLigaExpandido, cTagDesligaExpandido,
    cTagLigaAlturaDupla, cTagDesligaAlturaDupla,
    cTagLigaNegrito, cTagDesligaNegrito,
    cTagLigaSublinhado, cTagDesligaSublinhado,
    cTagLigaCondensado, cTagDesligaCondensado,
    cTagLigaItalico, cTagDesligaItalico,
    cTagFonteNormal);
  cTAGS_CARACTER_HELP: array[0..12] of String = (
    'Liga Expandido', 'Desliga Expandido',
    'Liga Altura Dupla', 'Desliga Altura Dupla',
    'Liga Negrito', 'Desliga Negrito',
    'Liga Sublinhado', 'Desliga Sublinhado',
    'Liga Condensado', 'Desliga Condensado',
    'Liga Italico', 'Desliga Italico',
    'Fonte Normal');

  cTagLinhaSimples = '</linha_simples>';
  cTagLinhaDupla   = '</linha_dupla>';
  cTagPuloDeLinhas = '</pular_linhas>';

  cTAGS_LINHAS: array[0..2] of String = (
    cTagLinhaSimples, cTagLinhaDupla, cTagPuloDeLinhas);
  cTAGS_LINHAS_HELP: array[0..2] of String = (
    'Imprime Linha Simples', 'Imprime Linha Dupla',
    'Pula N Linhas de acordo com propriedade do componente');

  cTagLogotipo = '</logo>';
  cTagLogoImprimir = '<logo_imprimir>';
  cTagLogoKC1 = '<logo_kc1>';
  cTagLogoKC2 = '<logo_kc2>';
  cTagLogoFatorX = '<logo_fatorx>';
  cTagLogoFatorY = '<logo_fatory>';

  cTagCorte = '</corte>';
  cTagCorteParcial = '</corte_parcial>';
  cTagCorteTotal = '</corte_total>';
  cTagAbreGaveta = '</abre_gaveta>';
  cTagAbreGavetaEsp = '<abre_gaveta>';
  cTagBeep = '</beep>';
  cTagZera = '</zera>';
  cTagReset = '</reset>';
  cTagPulodeLinha = '</lf>';
  cTagRetornoDeCarro = '</cr>';

  cTAGS_FUNCOES: array[0..9] of String = (
    cTagLogotipo,
    cTagCorte, cTagCorteParcial, cTagCorteTotal,
    cTagAbreGaveta,
    cTagBeep, CTagZera, cTagPulodeLinha, cTagRetornoDeCarro, cTagReset);
  cTAGS_FUNCOES_HELP: array[0..9] of String = (
    'Imprime Logotipo já gravado na Impressora (use utilitário do fabricante)',
    'Efetua Corte, conforme configuração de "TipoCorte"',
    'Efetua Corte Parcial no Papel (não disponivel em alguns modelos)',
    'Efetua Corte Total no papel',
    'Aciona a abertura da Gaveta de Dinheiro',
    'Emite um Beep na Impressora (não disponivel em alguns modelos)',
    'Reseta as configurações de Fonte Alinhamento.<LF>Ajusta Página de Código e Espaço entre Linhas',
    'Pula para a própxima linha',
    'Retorna para o Inicio da Linha',
    'Reseta as configurações de Fonte Alinhamento');

  cTagAlinhadoDireita = '<ad>';
  cTagAlinhadoEsquerda = '<ae>';
  cTagAlinhadoCentro = '<ce>';

  cTAGS_ALINHAMENTO: array[0..2] of String = (
    cTagAlinhadoDireita, cTagAlinhadoEsquerda, cTagAlinhadoCentro );
  cTAGS_ALINHAMENTO_HELP: array[0..2] of String = (
    'Texto Alinhado a Direita',
    'Texto Alinhado a Esquerda',
    'Texto Centralizado' );

  cTagBarraEAN8 = '<ean8>';
  cTagBarraEAN13 = '<ean13>';
  cTagBarraStd = '<std>';
  cTagBarraInter = '<inter>';
  cTagBarraCode11 = '<code11>';
  cTagBarraCode39 = '<code39>';
  cTagBarraCode93 = '<code93>';
  cTagBarraCode128 = '<code128>';
  cTagBarraCode128a = '<code128a>';
  cTagBarraCode128b = '<code128b>';
  cTagBarraCode128c = '<code128c>';
  cTagBarraUPCA = '<upca>';
  cTagBarraUPCE = '<upce>';
  cTagBarraCodaBar = '<codabar>';
  cTagBarraMSI = '<msi>';
  cTagBarraMostrar = '<barra_mostrar>';
  cTagBarraLargura = '<barra_largura>';
  cTagBarraAltura = '<barra_altura>';

  cTagQRCode = '<qrcode>';
  cTagQRCodeTipo = '<qrcode_tipo>';
  cTagQRCodeLargura = '<qrcode_largura>';
  cTagQRCodeError = '<qrcode_error>';

  cTagBMP = '<bmp>';

  cTagModoPaginaLiga       = '<mp>';
  cTagModoPaginaDesliga    = '</mp>';
  cTagModoPaginaImprimir   = '</mp_imprimir>';
  cTagModoPaginaDirecao    = '<mp_direcao>';
  cTagModoPaginaPosEsquerda= '<mp_esquerda>';
  cTagModoPaginaPosTopo    = '<mp_topo>';
  cTagModoPaginaLargura    = '<mp_largura>';
  cTagModoPaginaAltura     = '<mp_altura>';
  cTagModoPaginaEspaco     = '<mp_espaco>';
  cTagModoPaginaConfigurar = '</mp_configurar>';

  cTAGS_BARRAS: array[0..15] of String = (
    cTagBarraEAN8, cTagBarraEAN13, cTagBarraStd, cTagBarraInter, cTagBarraCode11,
    cTagBarraCode39, cTagBarraCode93, cTagBarraCode128, 
	cTagBarraUPCA, cTagBarraUPCE,
    cTagBarraCodaBar, cTagBarraMSI,
    cTagBarraCode128a, cTagBarraCode128b, cTagBarraCode128c, cTagQRCode);
  cTAGS_BARRAS_HELP: array[0..15] of String = (
    'Cod.Barras EAN8 - 7 numeros e 1 dig.verificador',
    'Cod.Barras EAN13 - 12 numeros e 1 dig.verificador',
    'Cod.Barras "Standard 2 of 5" - apenas números, tamanho livre',
    'Cod.Barras "Interleaved 2 of 5" - apenas números, tamanho PAR',
    'Cod.Barras Code11 - apenas números, tamanho livre',
    'Cod.Barras Code39 - Aceita: 0..9,A..Z, ,$,%,*,+,-,.,/, tamanho livre',
    'Cod.Barras Code93 - Aceita: 0..9,A..Z,-,., ,$,/,+,%, tamanho livre',
    'Cod.Barras Code128 - Todos os caracteres ASCII, tamanho livre',
    'Cod.Barras UPCA - 11 numeros e 1 dig.verificador',
    'Cod.Barras UPCE - 11 numeros e 1 dig.verificador',
    'Cod.Barras CodaBar - Aceita: 0..9,A..D,a..d,$,+,-,.,/,:, tamanho livre',
    'Cod.Barra MSI - Apenas números, 1 dígito verificador',
    'Cod.Barras Code128 - Subtipo A',
    'Cod.Barras Code128 - Subtipo B (padrão) = '+cTagBarraCode128,
    'Cod.Barras Code128 - Subtipo C (informar valores em BCD)',
    'Cod.Barras QrCode'
    );

  cTagIgnorarTags = '<ignorar_tags>';

  cACBrDeviceAtivarPortaException    = 'Porta não definida' ;
  cACBrDeviceAtivarException         = 'Erro abrindo: %s ' + sLineBreak +' %s ' ;
  cACBrDeviceAtivarPortaNaoEncontrada= 'Porta %s não encontrada' ;
  cACBrDeviceAtivarPortaNaoAcessivel = 'Porta %s não acessível' ;
  cACBrDeviceSetBaudException        = 'Valor deve estar na faixa de 50 a 4000000.'+#10+
                                       'Normalmente os equipamentos Seriais utilizam: 9600' ;
  cACBrDeviceSetDataException        = 'Valor deve estar na faixa de 5 a 8.'+#10+
                                       'Normalmente os equipamentos Seriais utilizam: 7 ou 8' ;
  cACBrDeviceSetPortaException       = 'Não é possível mudar a Porta com o Dispositivo Ativo' ;
  cACBrDeviceSetTypeException        = 'Tipo de dispositivo informado não condiz com o valor da Porta';
  cACBrDeviceSemImpressoraPadrao     = 'Erro Nenhuma impressora Padrão foi detectada';
  cACBrDeviceImpressoraNaoEncontrada = 'Impressora não encontrada [%s]';
  cACBrDeviceEnviaStrThreadException = 'Erro gravando em: %s ' ;
  cACBrDeviceEnviaStrFailCount       = 'Erro ao enviar dados para a porta: %s';

  { constantes para exibição na inicialização e no sobre do delphi a partir da versão 2009 }
  cACBrSobreDialogoTitulo = 'Projeto ACBr';
  cACBrSobreTitulo = 'Projeto ACBr VCL';
  cACBrSobreDescricao = 'ACBr VCL http://www.projetoacbr.com.br/' + #13#10 +
                        'Componentes para Automação Comercial' + #13#10 +                        
                        'Lesser General Public License version 2.0';					
  cACBrSobreLicencaStatus = 'LGPLv2';
  
  {****                                  *}
  
  {* Unit ACBrECFClass *}
  cACBrTempoInicioMsg                  = 3 ;  { Tempo para iniciar a exibiçao da mensagem Aguardando a Resposta da Impressora' }
  cACBrMsgAguardando                   = 'Aguardando a resposta da Impressora: %d segundos' ;
  cACBrMsgTrabalhando                  = 'Impressora está trabalhando' ;
  cACBrMsgPoucoPapel                   = 30 ; {Exibe alerta de Pouco Papel somente a cada 30 segundos}
  cACBrMsgRelatorio                    = 'Imprimindo %s  %dª Via ' ;
  cACBrPausaRelatorio                  = 5 ;
  cACBrMsgPausaRelatorio               = 'Destaque a %dª via, <ENTER> proxima, %d seg.';
  cACBrLinhasEntreCupons               = 7 ;
  cACBrMaxLinhasBuffer                 = 0 ;
  cACBrIntervaloAposComando            = 100 ; { Tempo em milisegundos a esperar apos o termino de EnviaComando }
  cACBrECFAliquotaSetTipoException     = 'Valores válidos para TACBrECFAliquota.Tipo: "T" - ICMS ou "S" - ISS' ;
  cACBrECFConsumidorCPFCNPJException   = 'CPF/CNPJ Não informado' ;
  cACBrECFConsumidorNomeException      = 'Para informar o Endereço é necessário informar o Nome' ;
  cACBrECFClassCreateException         = 'Essa Classe deve ser instanciada por TACBrECF' ;
  cACBrECFNaoInicializadoException     = 'Componente ACBrECF não está Ativo' ;
  cACBrECFOcupadoException             = 'Componente ACBrECF ocupado' + sLineBreak +
                                         'Aguardando resposta do comando anterior' ;
  cACBrECFSemRespostaException         = 'Impressora %s não está respondendo' ;
  cACBrECFSemPapelException            = 'FIM DE PAPEL' ;
  cACBrECFCmdSemRespostaException      = 'Comandos não estão sendo enviados para Impressora %s ' ;
  cACBrECFEnviaCmdSemRespostaException = 'Erro ao enviar comandos para a Impressora %s ' ;
  cACBrECFDoOnMsgSemRespostaRetentar   = 'A impressora %s não está repondendo.' ;
  cACBrECFVerificaFimLeituraException  = 'Erro Function VerificaFimLeitura não implementada em %s ' ;
  cACBrECFVerificaEmLinhaMsgRetentar   = 'A impressora %s não está pronta.' ;
  cACBrECFVerificaEmLinhaException     = 'Impressora %s não está em linha' ;
  cACBrECFPodeAbrirCupomRequerX        = 'A impressora %s requer Leitura X todo inicio de dia.'+#10+
                                         ' Imprima uma Leitura X para poder vender' ;
  cACBrECFPodeAbrirCupomRequerZ        = 'Redução Z de dia anterior não emitida.'+#10+
                                         ' Imprima uma Redução Z para poder vender' ;
  cACBrECFPodeAbrirCupomBloqueada      = 'Reduçao Z de hoje já emitida, ECF bloqueado até as 00:00' ;
  cACBrECFPodeAbrirCupomCFAberto       = 'Cupom Fiscal aberto' ;
  cACBrECFPodeAbrirCupomNaoAtivada     = 'Impressora nao foi Inicializada (Ativo = false)' ;
  cACBrECFPodeAbrirCupomEstado         = 'Estado da impressora %s  é '+sLineBreak+' %s (não é Livre) ' ;
  cACBrECFAbreGavetaException          = 'A Impressora %s não manipula Gavetas' ;
  cACBrECFImpactoAgulhasException      = 'A Impressora %s não permite ajustar o Impacto das Agulhas' ;
  cACBrECFImprimeChequeException       = 'Rotina de Impressão de Cheques não implementada para '+
                                         'Impressora %s ';
  cACBrECFLeituraCMC7Exception         = 'Rotina de Leitura de CMC7 de Cheques não implementada para '+
                                         'Impressora %s ';
  cACBrECFAchaCNFException             = 'Não existe nenhum Comprovante Não Fiscal '+
                                         'cadastrado como: "%s" ' ;
  cACBrECFAchaFPGException             = 'Não existe nenhuma Forma de Pagamento '+
                                         'cadastrada como: "%s" ' ;
  cACBrECFCMDInvalidoException         = 'Procedure: %s '+ sLineBreak +
                                         ' não implementada para a Impressora: %s'+sLineBreak + sLineBreak +
                                         'Ajude no desenvolvimento do ACBrECF. '+ sLineBreak+
                                         'Acesse nosso Forum em: http://acbr.sf.net/' ;
  cACBrECFDoOnMsgPoucoPapel            = 'Detectado pouco papel' ;
  cACBrECFDoOnMsgRetentar              = 'Deseja tentar novamente ?' ;
  cACBrECFAchaICMSAliquotaInvalida     = 'Aliquota inválida: ' ;
  cACBrECFAchaICMSCMDInvalido          = 'Aliquota não encontrada: ' ;
  cACBrECFAbrindoRelatorioGerencial    = 'Abrindo Relatório Gerencial, aguarde %d seg' ;
  cACBrECFFechandoRelatorioGerencial   = 'Fechando Relatório Gerencial' ;
  cACBrECFFormMsgDoProcedureException  = 'Erro. Chamada recurssiva de FormMsgDoProcedure' ;


  {* Unit ACBrECF *}
  cACBrECFSetModeloException             = 'Não é possível mudar o Modelo com o ECF Ativo' ;
  cACBrECFModeloNaoDefinidoException     = 'Modelo não definido' ;
  cACBrECFModeloBuscaPortaException      = 'Modelo: %s não consegue efetuar busca automática de Porta'+sLineBreak+
                                           'Favor definir a Porta Ex: (COM1, LPT1, /dev/lp0, etc)' ;
  cACBrECFMsgDoAcharPorta                = 'Procurando por ECF: %s na porta: %s ' ;
  cACBrECFSetDecimaisPrecoException      = 'Valor de DecimaisPreco deve estar entre 0-3' ;
  cACBrECFSetDecimaisQtdException        = 'Valor de DecimaisQtd deve estar entre 0-4' ;
  cACBrECFVendeItemQtdeException         = 'Quantidade deve ser superior a 0.' ;
  cACBrECFVendeItemValorUnitException    = 'Valor Unitario deve ser superior a 0.' ;
  cACBrECFVendeItemValDescAcreException  = 'ValorDescontoAcrescimo deve ser positivo' ;
  cACBrECFVendeItemDescAcreException     = 'DescontoAcrescimo deve ser "A"-Acrescimo, ou "D"-Desconto' ;
  cACBrECFVendeItemTipoDescAcreException = 'TipoDescontoAcrescimo deve ser "%"-Porcentagem, ou "$"-Valor' ;
  cACBrECFVendeItemAliqICMSException     = 'Aliquota de ICMS não pode ser vazia.' ;
  cACBrECFAchaFPGIndiceException         = 'Forma de Pagamento: %s inválida' ;
  cACBrECFFPGPermiteVinculadoException   = 'Forma de Pagamento: %s '+#10+
                                           'não permite Cupom Vinculado' ;
  cACBrECFPAFFuncaoNaoSuportada          = 'Função não suportada pelo modelo de ECF utilizado';
  cACBrECFRegistraItemNaoFiscalException = 'Comprovante não fiscal: %s inválido' ;
  cACBrECFSetRFDException                = 'Não é possível mudar ACBrECF.RFD com o componente ativo' ;
  cACBrECFSetAACException                = 'Não é possível mudar ACBrECF.AAC com o componente ativo' ;
  cACBrECFVirtualClassCreateException    = 'Essa Classe deve ser instanciada por TACBrECFVirtual' ;
  cACBrECFSetECFVirtualException         = 'Não é possível mudar ACBrECF.ECFVirtual com o componente ativo' ;
  cACBrECFSemECFVirtualException         = 'ACBrECF.ECFVirtual não foi atribuido' ;

  cACBrAACNumSerieNaoEncontardoException = 'ECF de Número de série %s não encontrado no Arquivo Auxiliar Criptografado.' ;
  cACBrAACValorGTInvalidoException       = 'Divergência no Valor do Grande Total.';

  cACBrDFeSSLEnviarException = 'Erro Interno: %d'+sLineBreak+'Erro HTTP: %d'+sLineBreak+
                               'URL: %s';
  cACBrArquivoNaoEncontrado = 'Arquivo: %s não encontrado';

  sDisplayFormat = ',#0.%.*d';

implementation

initialization
  // delphi XE3 em diante não possui mais essas var, então criar e preencher
  {$IFDEF DELPHI15_UP}
    fmtst := TFormatSettings.Create('');
    CurrencyString := fmtst.CurrencyString;
    CurrencyFormat := fmtst.CurrencyFormat;
    NegCurrFormat := fmtst.NegCurrFormat;
    ThousandSeparator := fmtst.ThousandSeparator;
    DecimalSeparator := fmtst.DecimalSeparator;
    CurrencyDecimals := fmtst.CurrencyDecimals;
    DateSeparator := fmtst.DateSeparator;
    ShortDateFormat := fmtst.ShortDateFormat;
    LongDateFormat := fmtst.LongDateFormat;
    TimeSeparator := fmtst.TimeSeparator;
    TimeAMString := fmtst.TimeAMString;
    TimePMString := fmtst.TimePMString;
    ShortTimeFormat := fmtst.ShortTimeFormat;
    LongTimeFormat := fmtst.LongTimeFormat;
    TwoDigitYearCenturyWindow := fmtst.TwoDigitYearCenturyWindow;
    ListSeparator := fmtst.ListSeparator;
  {$ENDIF}

end.
