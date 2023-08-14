{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliana Tamizou                                 }
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

unit ACBrBancoBic;

interface

uses
  Classes, SysUtils,
  ACBrBoleto, ACBrBoletoConversao;

type

  { TACBrBancoBic }

  TACBrBancoBic = class(TACBrBancoClass)
  private
  protected
  public
    Constructor create(AOwner: TACBrBanco);
    function CalcularDigitoVerificador(const ACBrTitulo:TACBrTitulo): String; override;
    function CalcularDigitoVerificadorArquivo(const ACBrTitulo:TACBrTitulo): String;
    function MontarCodigoBarras(const ACBrTitulo : TACBrTitulo): String; override;
    function MontarCampoNossoNumero(const ACBrTitulo :TACBrTitulo): String; override;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String; override;
    procedure GerarRegistroHeader400(NumeroRemessa : Integer; ARemessa:TStringList); override;
    procedure GerarRegistroTransacao400(ACBrTitulo : TACBrTitulo; aRemessa: TStringList); override;
    procedure GerarRegistroTrailler400(ARemessa:TStringList);  override;

    function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia) : String; override;
    function CodOcorrenciaToTipo(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
    function TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia):String; override;
    function CodMotivoRejeicaoToDescricao(const TipoOcorrencia:TACBrTipoOcorrencia; CodMotivo:Integer): String; override;

    function CodOcorrenciaToTipoRemessa(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
  end;

implementation

uses
  {$IFDEF COMPILER6_UP} dateutils {$ELSE} ACBrD5 {$ENDIF},
  StrUtils, ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings, ACBrUtil.DateTime;


{ TACBrBancoBic }

constructor TACBrBancoBic.create(AOwner: TACBrBanco);
begin
   inherited create(AOwner);
   fpDigito                := 2;
   fpNome                  := 'Bradesco';
   fpNumero                := 237;
   fpTamanhoMaximoNossoNum := 6;
   fpTamanhoAgencia        := 4;
   fpTamanhoConta          := 7;
   fpTamanhoCarteira       := 2;
end;

function TACBrBancoBic.CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo ): String;
begin
   Modulo.CalculoPadrao;
   Modulo.MultiplicadorFinal := 7;
   Modulo.Documento := ACBrTitulo.Carteira +
                       Copy(ACBrTitulo.ACBrBoleto.Cedente.Modalidade,2,2) +
                       ACBrTitulo.ACBrBoleto.Cedente.CodigoCedente +
                       ACBrTitulo.NossoNumero;
   Modulo.Calcular;

   if Modulo.ModuloFinal = 1 then
      Result:= 'P'
   else
      Result:= IntToStr(Modulo.DigitoFinal);
end;

function TACBrBancoBic.MontarCodigoBarras ( const ACBrTitulo: TACBrTitulo) : String;
var
  CodigoBarras, FatorVencimento, DigitoCodBarras:String;
begin
   with ACBrTitulo.ACBrBoleto do
   begin
      FatorVencimento := CalcularFatorVencimento(ACBrTitulo.Vencimento);

      CodigoBarras := IntToStr( Numero )+'9'+ FatorVencimento +
                      IntToStrZero(Round(ACBrTitulo.ValorDocumento*100),10) +
                      PadLeft(OnlyNumber(Cedente.Agencia),4,'0') +
                      ACBrTitulo.Carteira +
                      Copy(ACBrTitulo.ACBrBoleto.Cedente.Modalidade,2,2) +
                      ACBrTitulo.ACBrBoleto.Cedente.CodigoCedente +
                      ACBrTitulo.NossoNumero +
                      PadLeft(RightStr(Cedente.Conta,7),7,'0') + '0';

      DigitoCodBarras := CalcularDigitoCodigoBarras(CodigoBarras);
   end;

   Result:= IntToStr(Numero) + '9'+ DigitoCodBarras + Copy(CodigoBarras,5,39);
end;

function TACBrBancoBic.MontarCampoNossoNumero (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
   Result:= ACBrTitulo.Carteira+'/'+
            Copy(ACBrTitulo.ACBrBoleto.Cedente.Modalidade,2,2) +
            ACBrTitulo.ACBrBoleto.Cedente.CodigoCedente+
            ACBrTitulo.NossoNumero+
            '-'+
            CalcularDigitoVerificador(ACBrTitulo);
end;

function TACBrBancoBic.MontarCampoCodigoCedente (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
   Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia+'-'+
             ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito+'/'+
             ACBrTitulo.ACBrBoleto.Cedente.Conta+'-'+
             ACBrTitulo.ACBrBoleto.Cedente.ContaDigito;
end;

procedure TACBrBancoBic.GerarRegistroHeader400(NumeroRemessa : Integer; ARemessa:TStringList);
var
  wLinha: String;
begin
   with ACBrBanco.ACBrBoleto.Cedente do
   begin
      wLinha:= '0'                                             + // ID do Registro
               '1'                                             + // ID do Arquivo( 1 - Remessa)
               'REMESSA'                                       + // Literal de Remessa
               '02'                                            + // Layout versão 1.02
               PadRight( 'COBRANCA', 15 )                          + // Descrição do tipo de serviço
               PadLeft( CodigoTransmissao, 10, '0')               + // Codigo da Empresa no Banco
               Space(7)                                        + // Brancos
               PadLeft( Modalidade, 3, '0')                       + // Radical
               PadRight( Nome, 30)                                 + // Nome da Empresa
               IntToStr( 320 )+ PadRight('BICBANCO', 15)           + // Código e Nome do Banco(320 - BicBanco)
               FormatDateTime('ddmmyy',Now)                    + // Data de geração do arquivo
               '01600'                                         + // Densidade do arquivo
               'BPI'                                           + // Literal de densidade do arquivo
               IntToStrZero(NumeroRemessa,7) + Space(279)      + // Nr. Sequencial de Remessa + brancos
               IntToStrZero(1,6);                                // Nr. Sequencial de Remessa + brancos + Contador

      ARemessa.Text:= ARemessa.Text + UpperCase(wLinha);
   end;
end;

procedure TACBrBancoBic.GerarRegistroTransacao400(ACBrTitulo :TACBrTitulo; aRemessa: TStringList);
var
  DigitoNossoNumero, Ocorrencia, aEspecie, aAgencia, aDiasProtesto :String;
  Protesto, TipoSacado, TipoSacador, TipoSacadorAvalista: String;
  aCarteira, wLinha : String;
begin

   with ACBrTitulo do
   begin
      DigitoNossoNumero := CalcularDigitoVerificadorArquivo(ACBrTitulo);

      aAgencia := '00900'; 
      aCarteira:= PadLeft(trim(Carteira), 2, '0');
      if aCarteira = '09' then
        aCarteira := '4';



      {Pegando Código da Ocorrencia}
      case OcorrenciaOriginal.Tipo of
         toRemessaBaixar                         : Ocorrencia := '02'; {Pedido de Baixa}
         toRemessaConcederAbatimento             : Ocorrencia := '04'; {Concessão de Abatimento}
         toRemessaCancelarAbatimento             : Ocorrencia := '05'; {Cancelamento de Abatimento concedido}
         toRemessaAlterarVencimento              : Ocorrencia := '06'; {Alteração de vencimento}
         toRemessaAlterarNumeroControle          : Ocorrencia := '08'; {Alteração de seu número}
         toRemessaProtestar                      : Ocorrencia := '09'; {Pedido de protesto}
         toRemessaCancelarInstrucaoProtestoBaixa : Ocorrencia := '18'; {Sustar protesto e baixar}
         toRemessaCancelarInstrucaoProtesto      : Ocorrencia := '19'; {Sustar protesto e manter na carteira}
         toRemessaOutrasOcorrencias              : Ocorrencia := '31'; {Alteração de Outros Dados}
      else
         Ocorrencia := '01';                                           {Remessa}
      end;

      {Pegando Tipo de Boleto}
      if (ACBrBoleto.Cedente.ResponEmissao <> tbCliEmite) then
      begin
         if NossoNumero = EmptyStr then
           DigitoNossoNumero := '0';
      end;

      {Pegando Especie}
      if trim(EspecieDoc) = 'DM' then
         aEspecie:= '01'
      else if trim(EspecieDoc) = 'NP' then
         aEspecie:= '02'
      else if trim(EspecieDoc) = 'NS' then
         aEspecie:= '03'
      else if trim(EspecieDoc) = 'CS' then
         aEspecie:= '04'
      else if trim(EspecieDoc) = 'ND' then
         aEspecie:= '11'
      else if trim(EspecieDoc) = 'DS' then
         aEspecie:= '12'
      else
         aEspecie := EspecieDoc;

         aDiasProtesto := '  ';
      {Pegando campo Intruções}
      if (DataProtesto > 0) and (DataProtesto > Vencimento) then
      begin
         Protesto := '0700';
         aDiasProtesto := IntToStrZero(DaysBetween(DataProtesto,Vencimento),2);
      end
      else if Ocorrencia = '31' then
         Protesto := '9999'
      else
         Protesto := PadLeft(trim(Instrucao1),2,'0') + PadLeft(trim(Instrucao2),2,'0');

      {Pegando Tipo de Sacado}
      case Sacado.Pessoa of
         pFisica   : TipoSacado := '01';
         pJuridica : TipoSacado := '02';
      else
         TipoSacado := '99';
      end;

      {Pegando Tipo de Sacador}
      case ACBrBoleto.Cedente.TipoInscricao of
         pFisica   : TipoSacador := '01';
         pJuridica : TipoSacador := '02';
      else
         TipoSacador := '  ';
      end;

      if Trim(Sacado.Avalista) = '' then
        TipoSacadorAvalista := '  '
      else
        if Length(OnlyNumber(Sacado.Avalista)) = 14 then
          TipoSacadorAvalista := '02'
        else
          TipoSacadorAvalista := '01';

      with ACBrBoleto do
      begin
         wLinha:= '1'                                                     +  // ID Registro
                  TipoSacador                                             +  //Tipo da Empresa Sacadora
                  PadLeft(OnlyNumber(Cedente.CNPJCPF),15,'0')             +  //CNPJ/CPF da Empresa
                  Cedente.CodigoTransmissao                               +  // Código de Transmissão
                  space(9)                                                +  // Filler - 9 Brancos
                  space(25)                                               +  // Uso da Empresa
                  '0' + PadRight(NossoNumero + DigitoNossoNumero, 7, ' ') +  // 0 Fixo, Nosso Número + DV
                  space(37)                                               +  // Filler - 37 Brancos
                  aCarteira                                               +
                  Ocorrencia                                              +  // Ocorrência
                  PadRight(SeuNumero,10,' ')                              +  // Numero de Controle do Participante
                  FormatDateTime( 'ddmmyy', Vencimento)                   +
                  IntToStrZero( Round( ValorDocumento * 100 ), 13)        +
                  IntToStrZero(320,3)                                     +
                  aAgencia                                                +  //Agência Cobradora
                  aEspecie                                                +
                  'N'                                                     +  //Aceite, valor fixo N
                  FormatDateTime( 'ddmmyy', DataDocumento )               +  // Data de Emissão
                  Protesto                                                +
                  IntToStrZero( round(ValorMoraJuros * 100 ), 13)         +
                  IfThen(DataDesconto < EncodeDate(2000,01,01),'000000',
                         FormatDateTime( 'ddmmyy', DataDesconto))         +
                  IntToStrZero( round( ValorDesconto * 100 ), 13)         +
                  IntToStrZero( round( ValorIOF * 100 ), 13)              +
                  IntToStrZero( round( ValorAbatimento * 100 ), 13)       +
                  TipoSacado + PadLeft(OnlyNumber(Sacado.CNPJCPF),15,'0') +
                  PadRight( Sacado.NomeSacado, 40, ' ')                   +
                  PadRight( Sacado.Logradouro + Sacado.Numero, 40)        +
                  PadRight( Sacado.Bairro, 12 )                           +
                  PadRight( Sacado.CEP, 8 )                               +
                  PadRight( Sacado.Cidade , 15 )                          +
                  PadRight( Sacado.UF, 2)                                 +
                  IfThen( PercentualMulta > 0, '2', '3')                  +  // Indica se exite Multa ou não
                  FormatDateTime( 'ddmmyy', Vencimento + 1)               +
                  IntToStrZero( round( PercentualMulta * 100 ), 13)       +  // Percentual de Multa formatado com 2 casas decimais
                  space(19)                                               +
                  aDiasProtesto                                           +
                  '9';                                                       //Moeda, Fixo 9


         wLinha:= wLinha + IntToStrZero(aRemessa.Count + 1, 6); // Nº SEQÜENCIAL DO REGISTRO NO ARQUIVO
         wLinha:= wLinha +
                  sLineBreak +
                  '2' +
                  TipoSacador                                             +  //Tipo da Empresa Sacadora
                  PadLeft(OnlyNumber(Cedente.CNPJCPF),15,'0')                +  //CNPJ/CPF da Empresa
                  Cedente.CodigoTransmissao                               +  // Código de Transmissão
                  space(9)                                                +  // Filler - 9 Brancos
                  space(25)                                               +  // Uso da Empresa
                  '0' + PadRight(NossoNumero + DigitoNossoNumero, 7, ' ')     +  // 0 Fixo, Nosso Número + DV
                  space(37)                                               +  // Filler - 37 Brancos
                  PadRight( Sacado.NomeSacado, 40, ' ')                       +
                  TipoSacadorAvalista                                     +
                  PadLeft(OnlyNumber(Sacado.Avalista),15,'0')                +
                  PadRight('', 40, ' ')                                       +
                  Space(190)                                              +
                  IntToStrZero( aRemessa.Count + 2, 6);

         aRemessa.Text:= aRemessa.Text + UpperCase(wLinha);
      end;
   end;
end;

procedure TACBrBancoBic.GerarRegistroTrailler400( ARemessa:TStringList );
var
  wLinha: String;
begin
   wLinha := '9' + Space(393)                     + // ID Registro
             IntToStrZero( ARemessa.Count + 1, 6);  // Contador de Registros

   ARemessa.Text:= ARemessa.Text + UpperCase(wLinha);
end;

function TACBrBancoBic.TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String;
var
  CodOcorrencia: Integer;
begin
   CodOcorrencia := StrToIntDef(TipoOcorrenciaToCod(TipoOcorrencia),0);

   case CodOcorrencia of
     02: Result:='02-Entrada Confirmada' ;
     03: Result:='03-Entrada Rejeitada' ;
     06: Result:='06-Liquidação normal' ;
     09: Result:='09-Baixado Automaticamente via Arquivo' ;
     10: Result:='10-Baixado conforme instruções da Agência' ;
     11: Result:='11-Em Ser - Arquivo de Títulos pendentes' ;
     12: Result:='12-Abatimento Concedido' ;
     13: Result:='13-Abatimento Cancelado' ;
     14: Result:='14-Vencimento Alterado' ;
     15: Result:='15-Liquidação em Cartório' ;
     16: Result:= '16-Titulo Pago em Cheque - Vinculado';
     17: Result:='17-Liquidação após baixa ou Título não registrado' ;
     18: Result:='18-Acerto de Depositária' ;
     19: Result:='19-Confirmação Recebimento Instrução de Protesto' ;
     20: Result:='20-Confirmação Recebimento Instrução Sustação de Protesto' ;
     21: Result:='21-Acerto do Controle do Participante' ;
     22: Result:='22-Titulo com Pagamento Cancelado';
     23: Result:='23-Entrada do Título em Cartório' ;
     24: Result:='24-Entrada rejeitada por CEP Irregular' ;
     27: Result:='27-Baixa Rejeitada' ;
     28: Result:='28-Débito de tarifas/custas' ;
     29: Result:= '29-Ocorrências do Sacado';
     30: Result:='30-Alteração de Outros Dados Rejeitados' ;
     32: Result:='32-Instrução Rejeitada' ;
     33: Result:='33-Confirmação Pedido Alteração Outros Dados' ;
     34: Result:='34-Retirado de Cartório e Manutenção Carteira' ;
     35: Result:='35-Desagendamento do débito automático' ;
     40: Result:='40-Estorno de Pagamento';
     55: Result:='55-Sustado Judicial';
     68: Result:='68-Acerto dos dados do rateio de Crédito' ;
     69: Result:='69-Cancelamento dos dados do rateio' ;
   end;

   Result := ACBrSTr(Result);
end;

function TACBrBancoBic.CodOcorrenciaToTipo(const CodOcorrencia:
   Integer ) : TACBrTipoOcorrencia;
begin
   case CodOcorrencia of
      02: Result := toRetornoRegistroConfirmado;
      03: Result := toRetornoRegistroRecusado;
      06: Result := toRetornoLiquidado;
      09: Result := toRetornoBaixadoViaArquivo;
      10: Result := toRetornoBaixadoInstAgencia;
      11: Result := toRetornoTituloEmSer;
      12: Result := toRetornoAbatimentoConcedido;
      13: Result := toRetornoAbatimentoCancelado;
      14: Result := toRetornoVencimentoAlterado;
      15: Result := toRetornoLiquidadoEmCartorio;
      16: Result := toRetornoTituloPagoEmCheque;
      17: Result := toRetornoLiquidadoAposBaixaouNaoRegistro;
      18: Result := toRetornoAcertoDepositaria;
      19: Result := toRetornoRecebimentoInstrucaoProtestar;
      20: Result := toRetornoRecebimentoInstrucaoSustarProtesto;
      21: Result := toRetornoAcertoControleParticipante;
      22: Result := toRetornoTituloPagamentoCancelado;
      23: Result := toRetornoEncaminhadoACartorio;
      24: Result := toRetornoEntradaRejeitaCEPIrregular;
      27: Result := toRetornoBaixaRejeitada;
      28: Result := toRetornoDebitoTarifas;
      29: Result := toRetornoOcorrenciasdoSacado;
      30: Result := toRetornoAlteracaoOutrosDadosRejeitada;
      32: Result := toRetornoComandoRecusado;
      33: Result := toRetornoRecebimentoInstrucaoAlterarDados;
      34: Result := toRetornoRetiradoDeCartorio;
      35: Result := toRetornoDesagendamentoDebitoAutomatico;
      99: Result := toRetornoRegistroRecusado;
   else
      Result := toRetornoOutrasOcorrencias;
   end;
end;

function TACBrBancoBic.CodOcorrenciaToTipoRemessa(const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02 : Result:= toRemessaBaixar;                          {Pedido de Baixa}
    04 : Result:= toRemessaConcederAbatimento;              {Concessão de Abatimento}
    05 : Result:= toRemessaCancelarAbatimento;              {Cancelamento de Abatimento concedido}
    06 : Result:= toRemessaAlterarVencimento;               {Alteração de vencimento}
    08 : Result:= toRemessaAlterarNumeroControle;           {Alteração de seu número}
    09 : Result:= toRemessaProtestar;                       {Pedido de protesto}
    18 : Result:= toRemessaCancelarInstrucaoProtestoBaixa;  {Sustar protesto e baixar}
    19 : Result:= toRemessaCancelarInstrucaoProtesto;       {Sustar protesto e manter na carteira}
    31 : Result:= toRemessaOutrasOcorrencias;               {Alteração de Outros Dados}
  else
     Result:= toRemessaRegistrar;                           {Remessa}
  end;
end;

function TACBrBancoBic.TipoOcorrenciaToCod ( const TipoOcorrencia: TACBrTipoOcorrencia ) : String;
begin
  case TipoOcorrencia of
    toRetornoRegistroConfirmado                : Result := '02';
    toRetornoRegistroRecusado                  : Result := '03';
    toRetornoLiquidado                         : Result := '06';
    toRetornoBaixadoViaArquivo                 : Result := '09';
    toRetornoBaixadoInstAgencia                : Result := '10';
    toRetornoTituloEmSer                       : Result := '11';
    toRetornoAbatimentoConcedido               : Result := '12';
    toRetornoAbatimentoCancelado               : Result := '13';
    toRetornoVencimentoAlterado                : Result := '14';
    toRetornoLiquidadoEmCartorio               : Result := '15';
    toRetornoTituloPagoEmCheque                : Result := '16';
    toRetornoLiquidadoAposBaixaouNaoRegistro   : Result := '17';
    toRetornoAcertoDepositaria                 : Result := '18';
    toRetornoRecebimentoInstrucaoProtestar     : Result := '19';
    toRetornoRecebimentoInstrucaoSustarProtesto: Result := '20';
    toRetornoAcertoControleParticipante        : Result := '21';
    toRetornoTituloPagamentoCancelado          : Result := '22';
    toRetornoEncaminhadoACartorio              : Result := '23';
    toRetornoEntradaRejeitaCEPIrregular        : Result := '24';
    toRetornoBaixaRejeitada                    : Result := '27';
    toRetornoDebitoTarifas                     : Result := '28';
    toRetornoOcorrenciasDoSacado               : Result := '29';
    toRetornoAlteracaoOutrosDadosRejeitada     : Result := '30';
    toRetornoComandoRecusado                   : Result := '32';
    { DONE -oJacinto -cAjuste : Acrescentar a ocorrência correta referente ao código. }
    toRetornoRecebimentoInstrucaoAlterarDados  : Result := '33';
    { DONE -oJacinto -cAjuste : Acrescentar a ocorrência correta referente ao código. }
    toRetornoRetiradoDeCartorio                : Result := '34';
    toRetornoDesagendamentoDebitoAutomatico    : Result := '35';
  else
    Result := '01';
  end;
end;

function TACBrBancoBic.CalcularDigitoVerificadorArquivo(
  const ACBrTitulo: TACBrTitulo): String;
begin
   Modulo.CalculoPadrao;
   Modulo.MultiplicadorFinal := 9;
   Modulo.Documento := ACBrTitulo.ACBrBoleto.Cedente.Modalidade + ACBrTitulo.NossoNumero;
   Modulo.Calcular;

   if (Modulo.ModuloFinal = 0) or (Modulo.ModuloFinal = 10)then
      Result:= '1'
   else
   if Modulo.ModuloFinal = 1 then
      Result:= '0'
   else
      Result:= IntToStr(Modulo.DigitoFinal);
end;

function TACBrBancoBic.CodMotivoRejeicaoToDescricao( const TipoOcorrencia:TACBrTipoOcorrencia ;CodMotivo: Integer) : String;
begin
   case TipoOcorrencia of
      toRetornoRegistroConfirmado:
      case CodMotivo  of
         00: Result := '00-Ocorrencia aceita';
         01: Result := '01-Codigo de banco inválido';
         04: Result := '04-Cod. movimentacao nao permitido p/ a carteira';
         15: Result := '15-Caracteristicas de Cobranca Imcompativeis';
         17: Result := '17-Data de vencimento anterior a data de emissão';
         21: Result := '21-Espécie do Título inválido';
         24: Result := '24-Data da emissão inválida';
         38: Result := '38-Prazo para protesto inválido';
         39: Result := '39-Pedido para protesto não permitido para título';
         43: Result := '43-Prazo para baixa e devolução inválido';
         45: Result := '45-Nome do Sacado inválido';
         46: Result := '46-Tipo/num. de inscrição do Sacado inválidos';
         47: Result := '47-Endereço do Sacado não informado';
         48: Result := '48-CEP invalido';
         50: Result := '50-CEP referente a Banco correspondente';
         53: Result := '53-Nº de inscrição do Sacador/avalista inválidos (CPF/CNPJ)';
         54: Result := '54-Sacador/avalista não informado';
         67: Result := '67-Débito automático agendado';
         68: Result := '68-Débito não agendado - erro nos dados de remessa';
         69: Result := '69-Débito não agendado - Sacado não consta no cadastro de autorizante';
         70: Result := '70-Débito não agendado - Cedente não autorizado pelo Sacado';
         71: Result := '71-Débito não agendado - Cedente não participa da modalidade de débito automático';
         72: Result := '72-Débito não agendado - Código de moeda diferente de R$';
         73: Result := '73-Débito não agendado - Data de vencimento inválida';
         75: Result := '75-Débito não agendado - Tipo do número de inscrição do sacado debitado inválido';
         86: Result := '86-Seu número do documento inválido';
         89: Result := '89-Email sacado nao enviado - Titulo com debito automatico';
         90: Result := '90-Email sacado nao enviado - Titulo com cobranca sem registro';
      else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
      toRetornoRegistroRecusado:
      case CodMotivo of
         02: Result:= '02-Codigo do registro detalhe invalido';
         03: Result:= '03-Codigo da Ocorrencia Invalida';
         04: Result:= '04-Codigo da Ocorrencia nao permitida para a carteira';
         05: Result:= '05-Codigo de Ocorrencia nao numerico';
         07: Result:= 'Agencia\Conta\Digito invalido';
         08: Result:= 'Nosso numero invalido';
         09: Result:= 'Nosso numero duplicado';
         10: Result:= 'Carteira invalida';
         13: Result:= 'Idetificacao da emissao do boleto invalida';
         16: Result:= 'Data de vencimento invalida';
         18: Result:= 'Vencimento fora do prazo de operacao';
         20: Result:= 'Valor do titulo invalido';
         21: Result:= 'Especie do titulo invalida';
         22: Result:= 'Especie nao permitida para a carteira';
         24: Result:= 'Data de emissao invalida';
         28: Result:= 'Codigo de desconto invalido';
         38: Result:= 'Prazo para protesto invalido';
         44: Result:= 'Agencia cedente nao prevista';
         45: Result:= 'Nome cedente nao informado';
         46: Result:= 'Tipo/numero inscricao sacado invalido';
         47: Result:= 'Endereco sacado nao informado';
         48: Result:= 'CEP invalido';
         50: Result:= 'CEP irregular - Banco correspondente';
         63: Result:= 'Entrada para titulo ja cadastrado';
         65: Result:= 'Limite excedido';
         66: Result:= 'Numero autorizacao inexistente';
         68: Result:= 'Debito nao agendado - Erro nos dados da remessa';
         69: Result:= 'Debito nao agendado - Sacado nao consta no cadastro de autorizante';
         70: Result:= 'Debito nao agendado - Cedente nao autorizado pelo sacado';
         71: Result:= 'Debito nao agendado - Cedente nao participa de debito automatico';
         72: Result:= 'Debito nao agendado - Codigo de moeda diferente de R$';
         73: Result:= 'Debito nao agendado - Data de vencimento invalida';
         74: Result:= 'Debito nao agendado - Conforme seu pedido titulo nao registrado';
         75: Result:= 'Debito nao agendado - Tipo de numero de inscricao de debitado invalido';
      else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
      toRetornoLiquidado:
      case CodMotivo of
         00: Result:= '00-Titulo pago com dinheiro';
         15: Result:= '15-Titulo pago com cheque';
         42: Result:= '42-Rateio nao efetuado';
      else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
      toRetornoBaixadoViaArquivo:
      case CodMotivo of
         00: Result:= '00-Ocorrencia aceita';
         10: Result:= '10=Baixa comandada pelo cliente';
      else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
      toRetornoBaixadoInstAgencia:
         case CodMotivo of
            00: Result:= '00-Baixado conforme instrucoes na agencia';
            14: Result:= '14-Titulo protestado';
            15: Result:= '15-Titulo excluido';
            16: Result:= '16-Titulo baixado pelo banco por decurso de prazo';
            20: Result:= '20-Titulo baixado e transferido para desconto';
         else
            Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
         end;
      toRetornoLiquidadoAposBaixaouNaoRegistro:
      case CodMotivo of
         00: Result:= '00-Pago com dinheiro';
         15: Result:= '15-Pago com cheque';
      else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

      toRetornoLiquidadoEmCartorio:
      case CodMotivo of
         00: Result:= '00-Pago com dinheiro';
         15: Result:= '15-Pago com cheque';
      else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

      toRetornoEntradaRejeitaCEPIrregular:
      case CodMotivo of
         48: Result:= '48-CEP invalido';
      else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

      toRetornoBaixaRejeitada:
      case CodMotivo of
         04: Result:= '04-Codigo de ocorrencia nao permitido para a carteira';
         07: Result:= '07-Agencia\Conta\Digito invalidos';
         08: Result:= '08-Nosso numero invalido';
         10: Result:= '10-Carteira invalida';
         15: Result:= '15-Carteira\Agencia\Conta\NossoNumero invalidos';
         40: Result:= '40-Titulo com ordem de protesto emitido';
         42: Result:= '42-Codigo para baixa/devolucao via Telebradesco invalido';
         60: Result:= '60-Movimento para titulo nao cadastrado';
         77: Result:= '70-Transferencia para desconto nao permitido para a carteira';
         85: Result:= '85-Titulo com pagamento vinculado';
      else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

      toRetornoDebitoTarifas:
      case CodMotivo of
         02: Result:= '02-Tarifa de permanência título cadastrado';
         03: Result:= '03-Tarifa de sustação';
         04: Result:= '04-Tarifa de protesto';
         05: Result:= '05-Tarifa de outras instrucoes';
         06: Result:= '06-Tarifa de outras ocorrências';
         08: Result:= '08-Custas de protesto';
         12: Result:= '12-Tarifa de registro';
         13: Result:= '13-Tarifa titulo pago no Bradesco';
         14: Result:= '14-Tarifa titulo pago compensacao';
         15: Result:= '15-Tarifa título baixado não pago';
         16: Result:= '16-Tarifa alteracao de vencimento';
         17: Result:= '17-Tarifa concessão abatimento';
         18: Result:= '18-Tarifa cancelamento de abatimento';
         19: Result:= '19-Tarifa concessão desconto';
         20: Result:= '20-Tarifa cancelamento desconto';
         21: Result:= '21-Tarifa título pago cics';
         22: Result:= '22-Tarifa título pago Internet';
         23: Result:= '23-Tarifa título pago term. gerencial serviços';
         24: Result:= '24-Tarifa título pago Pág-Contas';
         25: Result:= '25-Tarifa título pago Fone Fácil';
         26: Result:= '26-Tarifa título Déb. Postagem';
         27: Result:= '27-Tarifa impressão de títulos pendentes';
         28: Result:= '28-Tarifa título pago BDN';
         29: Result:= '29-Tarifa título pago Term. Multi Funcao';
         30: Result:= '30-Impressão de títulos baixados';
         31: Result:= '31-Impressão de títulos pagos';
         32: Result:= '32-Tarifa título pago Pagfor';
         33: Result:= '33-Tarifa reg/pgto – guichê caixa';
         34: Result:= '34-Tarifa título pago retaguarda';
         35: Result:= '35-Tarifa título pago Subcentro';
         36: Result:= '36-Tarifa título pago Cartao de Credito';
         37: Result:= '37-Tarifa título pago Comp Eletrônica';
         38: Result:= '38-Tarifa título Baix. Pg. Cartorio';
         39: Result:='39-Tarifa título baixado acerto BCO';
         40: Result:='40-Baixa registro em duplicidade';
         41: Result:='41-Tarifa título baixado decurso prazo';
         42: Result:='42-Tarifa título baixado Judicialmente';
         43: Result:='43-Tarifa título baixado via remessa';
         44: Result:='44-Tarifa título baixado rastreamento';
         45: Result:='45-Tarifa título baixado conf. Pedido';
         46: Result:='46-Tarifa título baixado protestado';
         47: Result:='47-Tarifa título baixado p/ devolucao';
         48: Result:='48-Tarifa título baixado franco pagto';
         49: Result:='49-Tarifa título baixado SUST/RET/CARTÓRIO';
         50: Result:='50-Tarifa título baixado SUS/SEM/REM/CARTÓRIO';
         51: Result:='51-Tarifa título transferido desconto';
         52: Result:='52-Cobrado baixa manual';
         53: Result:='53-Baixa por acerto cliente';
         54: Result:='54-Tarifa baixa por contabilidade';
         55: Result:='55-BIFAX';
         56: Result:='56-Consulta informações via internet';
         57: Result:='57-Arquivo retorno via internet';
         58: Result:='58-Tarifa emissão Papeleta';
         59: Result:='59-Tarifa fornec papeleta semi preenchida';
         60: Result:='60-Acondicionador de papeletas (RPB)S';
         61: Result:='61-Acond. De papelatas (RPB)s PERSONAL';
         62: Result:='62-Papeleta formulário branco';
         63: Result:='63-Formulário A4 serrilhado';
         64: Result:='64-Fornecimento de softwares transmiss';
         65: Result:='65-Fornecimento de softwares consulta';
         66: Result:='66-Fornecimento Micro Completo';
         67: Result:='67-Fornecimento MODEN';
         68: Result:='68-Fornecimento de máquina FAX';
         69: Result:='69-Fornecimento de maquinas oticas';
         70: Result:='70-Fornecimento de Impressoras';
         71: Result:='71-Reativação de título';
         72: Result:='72-Alteração de produto negociado';
         73: Result:='73-Tarifa emissao de contra recibo';
         74: Result:='74-Tarifa emissao 2ª via papeleta';
         75: Result:='75-Tarifa regravação arquivo retorno';
         76: Result:='76-Arq. Títulos a vencer mensal';
         77: Result:='77-Listagem auxiliar de crédito';
         78: Result:='78-Tarifa cadastro cartela instrução permanente';
         79: Result:='79-Canalização de Crédito';
         80: Result:='80-Cadastro de Mensagem Fixa';
         81: Result:='81-Tarifa reapresentação automática título';
         82: Result:='82-Tarifa registro título déb. Automático';
         83: Result:='83-Tarifa Rateio de Crédito';
         84: Result:='84-Emissão papeleta sem valor';
         85: Result:='85-Sem uso';
         86: Result:='86-Cadastro de reembolso de diferença';
         87: Result:='87-Relatório fluxo de pagto';
         88: Result:='88-Emissão Extrato mov. Carteira';
         89: Result:='89-Mensagem campo local de pagto';
         90: Result:='90-Cadastro Concessionária serv. Publ.';
         91: Result:='91-Classif. Extrato Conta Corrente';
         92: Result:='92-Contabilidade especial';
         93: Result:='93-Realimentação pagto';
         94: Result:='94-Repasse de Créditos';
         95: Result:='95-Tarifa reg. pagto Banco Postal';
         96: Result:='96-Tarifa reg. Pagto outras mídias';
         97: Result:='97-Tarifa Reg/Pagto – Net Empresa';
         98: Result:='98-Tarifa título pago vencido';
         99: Result:='99-TR Tít. Baixado por decurso prazo';
         100: Result:='100-Arquivo Retorno Antecipado';
         101: Result:='101-Arq retorno Hora/Hora';
         102: Result:='102-TR. Agendamento Déb Aut';
         103: Result:='103-TR. Tentativa cons Déb Aut';
         104: Result:='104-TR Crédito on-line';
         105: Result:='105-TR. Agendamento rat. Crédito';
         106: Result:='106-TR Emissão aviso rateio';
         107: Result:='107-Extrato de protesto';
         110: Result:='110-Tarifa reg/pagto Bradesco Expresso';
      else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

      toRetornoOcorrenciasdoSacado:
      case CodMotivo of
         78 : Result:= '78-Sacado alega que faturamento e indevido';
         116: Result:= '116-Sacado aceita/reconhece o faturamento';
      else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

      toRetornoALteracaoOutrosDadosRejeitada:
      case CodMotivo of
         01: Result:= '01-Código do Banco inválido';
         04: Result:= '04-Código de ocorrência não permitido para a carteira';
         05: Result:= '05-Código da ocorrência não numérico';
         08: Result:= '08-Nosso número inválido';
         15: Result:= '15-Característica da cobrança incompatível';
         16: Result:= '16-Data de vencimento inválido';
         17: Result:= '17-Data de vencimento anterior a data de emissão';
         18: Result:= '18-Vencimento fora do prazo de operação';
         24: Result:= '24-Data de emissão Inválida';
         26: Result:= '26-Código de juros de mora inválido';
         27: Result:= '27-Valor/taxa de juros de mora inválido';
         28: Result:= '28-Código de desconto inválido';
         29: Result:= '29-Valor do desconto maior/igual ao valor do Título';
         30: Result:= '30-Desconto a conceder não confere';
         31: Result:= '31-Concessão de desconto já existente ( Desconto anterior )';
         32: Result:= '32-Valor do IOF inválido';
         33: Result:= '33-Valor do abatimento inválido';
         34: Result:= '34-Valor do abatimento maior/igual ao valor do Título';
         38: Result:= '38-Prazo para protesto inválido';
         39: Result:= '39-Pedido de protesto não permitido para o Título';
         40: Result:= '40-Título com ordem de protesto emitido';
         42: Result:= '42-Código para baixa/devolução inválido';
         46: Result:= '46-Tipo/número de inscrição do sacado inválidos';
         48: Result:= '48-Cep Inválido';
         53: Result:= '53-Tipo/Número de inscrição do sacador/avalista inválidos';
         54: Result:= '54-Sacador/avalista não informado';
         57: Result:= '57-Código da multa inválido';
         58: Result:= '58-Data da multa inválida';
         60: Result:= '60-Movimento para Título não cadastrado';
         79: Result:= '79-Data de Juros de mora Inválida';
         80: Result:= '80-Data do desconto inválida';
         85: Result:= '85-Título com Pagamento Vinculado.';
         88: Result:= '88-E-mail Sacado não lido no prazo 5 dias';
         91: Result:= '91-E-mail sacado não recebido';
      else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

      toRetornoComandoRecusado:
      case CodMotivo of
         01 : Result:= '01-Código do Banco inválido';
         02 : Result:= '02-Código do registro detalhe inválido';
         04 : Result:= '04-Código de ocorrência não permitido para a carteira';
         05 : Result:= '05-Código de ocorrência não numérico';
         07 : Result:= '07-Agência/Conta/dígito inválidos';
         08 : Result:= '08-Nosso número inválido';
         10 : Result:= '10-Carteira inválida';
         15 : Result:= '15-Características da cobrança incompatíveis';
         16 : Result:= '16-Data de vencimento inválida';
         17 : Result:= '17-Data de vencimento anterior a data de emissão';
         18 : Result:= '18-Vencimento fora do prazo de operação';
         20 : Result:= '20-Valor do título inválido';
         21 : Result:= '21-Espécie do Título inválida';
         22 : Result:= '22-Espécie não permitida para a carteira';
         24 : Result:= '24-Data de emissão inválida';
         28 : Result:= '28-Código de desconto via Telebradesco inválido';
         29 : Result:= '29-Valor do desconto maior/igual ao valor do Título';
         30 : Result:= '30-Desconto a conceder não confere';
         31 : Result:= '31-Concessão de desconto - Já existe desconto anterior';
         33 : Result:= '33-Valor do abatimento inválido';
         34 : Result:= '34-Valor do abatimento maior/igual ao valor do Título';
         36 : Result:= '36-Concessão abatimento - Já existe abatimento anterior';
         38 : Result:= '38-Prazo para protesto inválido';
         39 : Result:= '39-Pedido de protesto não permitido para o Título';
         40 : Result:= '40-Título com ordem de protesto emitido';
         41 : Result:= '41-Pedido cancelamento/sustação para Título sem instrução de protesto';
         42 : Result:= '42-Código para baixa/devolução inválido';
         45 : Result:= '45-Nome do Sacado não informado';
         46 : Result:= '46-Tipo/número de inscrição do Sacado inválidos';
         47 : Result:= '47-Endereço do Sacado não informado';
         48 : Result:= '48-CEP Inválido';
         50 : Result:= '50-CEP referente a um Banco correspondente';
         53 : Result:= '53-Tipo de inscrição do sacador avalista inválidos';
         60 : Result:= '60-Movimento para Título não cadastrado';
         85 : Result:= '85-Título com pagamento vinculado';
         86 : Result:= '86-Seu número inválido';
         94 : Result:= '94-Título Penhorado – Instrução Não Liberada pela Agência';

      else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

      toRetornoDesagendamentoDebitoAutomatico:
      case CodMotivo of
         81 : Result:= '81-Tentativas esgotadas, baixado';
         82 : Result:= '82-Tentativas esgotadas, pendente';
         83 : Result:= '83-Cancelado pelo Sacado e Mantido Pendente, conforme negociação';
         84 : Result:= '84-Cancelado pelo sacado e baixado, conforme negociação';
      else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
   else
      Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
   end;

   Result := ACBrSTr(Result);
end;


end.


