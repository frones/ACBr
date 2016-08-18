{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2009 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:   Juliana Rodrigues Prado                       }
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
******************************************************************************}

{$I ACBr.inc}

unit ACBrBancoCecred;

interface

uses
  Classes, SysUtils,
  ACBrBoleto;

const
  CACBrBancoCecred_Versao = '0.0.1';

type
  { TACBrBancoCecred}

  TACBrBancoCecred = class(TACBrBancoClass)
   protected
   private
    function FormataNossoNumero(const ACBrTitulo :TACBrTitulo): String;
   public
    Constructor create(AOwner: TACBrBanco);
    function CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo ): String; override;
    function MontarCodigoBarras(const ACBrTitulo : TACBrTitulo): String; override;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String; override;
    function MontarCampoNossoNumero(const ACBrTitulo :TACBrTitulo): String; override;
    procedure GerarRegistroHeader400(NumeroRemessa : Integer; aRemessa:TStringList); override;
    procedure GerarRegistroTransacao400(ACBrTitulo : TACBrTitulo; aRemessa: TStringList); override;
    procedure GerarRegistroTrailler400(ARemessa : TStringList);  override;
    function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia) : String; override;
    function CodOcorrenciaToTipo(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
    function TipoOCorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia):String; override;
    procedure LerRetorno240(ARetorno:TStringList); override;
    procedure LerRetorno400(ARetorno: TStringList); override;
    function CodMotivoRejeicaoToDescricao(
      const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: Integer): String; override;

    function CalcularTamMaximoNossoNumero(const Carteira : String; NossoNumero : String = ''; Convenio: String = ''): Integer; override;
   end;

implementation

uses
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF},
  StrUtils, Variants,
  ACBrValidador, ACBrUtil;

constructor TACBrBancoCecred.create(AOwner: TACBrBanco);
begin
   inherited create(AOwner);
   fpDigito := 1;
   fpNome   := 'Banco Cecred';
   fpNumero := 085;
   fpTamanhoMaximoNossoNum := 17;
   fpTamanhoConta   := 8;
   fpTamanhoAgencia := 4;
   fpTamanhoCarteira:= 2;
end;

function TACBrBancoCecred.CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo ): String;
begin
   Result := '0';

   Modulo.CalculoPadrao;
   Modulo.MultiplicadorFinal   := 2;
   Modulo.MultiplicadorInicial := 9;
   Modulo.Documento := FormataNossoNumero(ACBrTitulo);
   Modulo.Calcular;

   if Modulo.ModuloFinal >= 10 then
      Result:= '1'
   else
      Result:= IntToStr(Modulo.ModuloFinal);
end;

function TACBrBancoCecred.CalcularTamMaximoNossoNumero(
  const Carteira: String; NossoNumero : String = ''; Convenio: String = ''): Integer;
begin
   Result := 17;

   if (ACBrBanco.ACBrBoleto.Cedente.Conta = '') then
      raise Exception.Create(ACBrStr('Banco Cecred requer que a Conta '+
                                     'seja informada.'));

end;

function TACBrBancoCecred.FormataNossoNumero(const ACBrTitulo :TACBrTitulo): String;
var
  ANossoNumero : String;
  AConta : String;
begin


   with ACBrTitulo do
   begin


      AConta    := IntToStr(StrToInt64(OnlyNumber(ACBrBoleto.Cedente.Conta + ACBrBoleto.Cedente.ContaDigito)));
      ANossoNumero := IntToStr(StrToInt64(OnlyNumber(NossoNumero)));

      ANossoNumero := PadLeft(AConta, 8, '0') + PadLeft(ANossoNumero, 9, '0')
   end;
   Result := ANossoNumero;
end;


function TACBrBancoCecred.MontarCodigoBarras(const ACBrTitulo : TACBrTitulo): String;
var
  CodigoBarras: String;
  FatorVencimento: String;
  DigitoCodBarras: String;
  ANossoNumero: String;
  AConvenio: String;
begin
   AConvenio    := IntToStr(StrToInt64(OnlyNumber(ACBrTitulo.ACBrBoleto.Cedente.Convenio)));
   ANossoNumero := IntToStrZero(StrToIntDef(OnlyNumber(ACBrTitulo.NossoNumero),0),9);

   {Codigo de Barras}
   with ACBrTitulo.ACBrBoleto do
   begin
      FatorVencimento := CalcularFatorVencimento(ACBrTitulo.Vencimento);
      CodigoBarras := IntToStrZero(Banco.Numero, 3) +
                      '9' +
                      FatorVencimento +
                      IntToStrZero(Round(ACBrTitulo.ValorDocumento * 100), 10) +
                      PadLeft(AConvenio, 6, '0') +
                      IntToStrZero(StrToIntDef(OnlyNumber(Cedente.Conta + Cedente.ContaDigito),0),8) +
                      ANossoNumero +
                      PadRight(ACBrTitulo.Carteira, fpTamanhoCarteira, '0');

      DigitoCodBarras := CalcularDigitoCodigoBarras(CodigoBarras);
   end;

   Result:= copy( CodigoBarras, 1, 4) + DigitoCodBarras + copy( CodigoBarras, 5, 44) ;
end;

function TACBrBancoCecred.MontarCampoCodigoCedente (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
   Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia+'-'+
             ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito+'/'+
             IntToStrZero(StrToIntDef(ACBrTitulo.ACBrBoleto.Cedente.Conta,0),8)+'-'+
             ACBrTitulo.ACBrBoleto.Cedente.ContaDigito;
end;

function TACBrBancoCecred.MontarCampoNossoNumero (const ACBrTitulo: TACBrTitulo ) : String;
var
  ANossoNumero: String;
begin
   ANossoNumero := FormataNossoNumero(ACBrTitulo);
   Result:= ANossoNumero

end;

procedure TACBrBancoCecred.GerarRegistroHeader400(NumeroRemessa: Integer; aRemessa:TStringList);
var
  aAgencia: String;
  aConta: String;
  aConvenio : String;
  wLinha: String;
begin

   with ACBrBanco.ACBrBoleto.Cedente do
   begin
      aAgencia:= IntToStrZero(StrToIntDef(OnlyNumber(Agencia),0),4);
      aConta  := IntToStrZero(StrToIntDef(OnlyNumber(Conta),0),8);
      aConvenio := IntToStrZero(StrToIntDef(OnlyNumber(Convenio),0),7);

      wLinha:= '0'                            + // ID do Registro
               '1'                            + // ID do Arquivo( 1 - Remessa)
               'REMESSA'                      + // Literal de Remessa
               '01'                           + // Código do Tipo de Serviço
               'COBRANCA'                     + // Descrição do tipo de serviço
               Space(7)                       + // Brancos
               aAgencia                       + // Prefixo da agência/ onde esta cadastrado o convenente lider do cedente
               PadRight( AgenciaDigito, 1, ' ')   + // DV-prefixo da agência
               aConta                         + // Codigo do cedente/nr. da conta corrente que está cadastro o convenio lider do cedente
               PadRight( ContaDigito, 1, ' ')     + // DV-código do cedente
               '000000'                       + // Complemento
               PadRight( Nome, 30)                + // Nome da Empresa
               PadRight( '085CECRED',18,' ')      + // Identificador do Banco
               FormatDateTime('ddmmyy',Now)   + // Data de geração do arquivo
               IntToStrZero(NumeroRemessa,7)  + // Numero Remessa
               Space(22)                      + // Brancos
               aConvenio                      + // Nr. Convênio
               space(258)                     + // Brancos
               IntToStrZero(1,6);               // Nr. Sequencial do registro-informar 000001

      aRemessa.Text:= aRemessa.Text + UpperCase(wLinha);
   end;
end;

procedure TACBrBancoCecred.GerarRegistroTransacao400(ACBrTitulo: TACBrTitulo; aRemessa: TStringList);
var
  ANossoNumero: String;
  ADigitoNossoNumero: String;
  ATipoOcorrencia: String;
  AInstrucao: String;
  ATipoSacado: String;
  ATipoCendente: String;
  ATipoAceite: String;
  ATipoEspecieDoc: String;
  AMensagem: String;
  DiasProtesto: String;
  aAgencia: String;
  aConta: String;
  aModalidade: String;
  wLinha: String;
  aTipoCobranca: String;
begin

   with ACBrTitulo do
   begin
      ANossoNumero := FormataNossoNumero(ACBrTitulo);
      ADigitoNossoNumero := CalcularDigitoVerificador(ACBrTitulo);
      aAgencia:= IntToStrZero(StrToIntDef(OnlyNumber(ACBrBoleto.Cedente.Agencia),0),4);
      aConta  := IntToStrZero(StrToIntDef(OnlyNumber(ACBrBoleto.Cedente.Conta),0),8);
      aModalidade := IntToStrZero(StrToIntDef(trim(ACBrBoleto.Cedente.Modalidade),0),3);
       
      {Pegando Código da Ocorrencia}
      case OcorrenciaOriginal.Tipo of
         toRemessaBaixar                         : ATipoOcorrencia := '02'; {Pedido de Baixa}
         toRemessaConcederAbatimento             : ATipoOcorrencia := '04'; {Concessão de Abatimento}
         toRemessaCancelarAbatimento             : ATipoOcorrencia := '05'; {Cancelamento de Abatimento concedido}
         toRemessaAlterarVencimento              : ATipoOcorrencia := '06'; {Alteração de vencimento}
         toRemessaAlterarControleParticipante    : ATipoOcorrencia := '07'; {Alteração do número de controle do participante}
         toRemessaAlterarNumeroControle          : ATipoOcorrencia := '08'; {Alteração de seu número}
         toRemessaProtestar                      : ATipoOcorrencia := '09'; {Pedido de protesto}
         toRemessaCancelarInstrucaoProtestoBaixa : ATipoOcorrencia := '10'; {Sustar protesto e baixar}
         toRemessaCancelarInstrucaoProtesto      : ATipoOcorrencia := '10'; {Sustar protesto e manter na carteira}
         toRemessaDispensarJuros                 : ATipoOcorrencia := '11'; {Instrução para dispensar juros}
         toRemessaAlterarNomeEnderecoSacado      : ATipoOcorrencia := '12'; {Alteração de nome e endereço do Sacado}
         toRemessaOutrasOcorrencias              : ATipoOcorrencia := '31'; {Conceder desconto}
         toRemessaCancelarDesconto               : ATipoOcorrencia := '32'; {Não conceder desconto}
      else
         ATipoOcorrencia := '01';                                           {Registro de títulos}
      end;

      { Pegando o Aceite do Titulo }
      case Aceite of
         atSim :  ATipoAceite := 'A';
         atNao :  ATipoAceite := 'N';
      end;

      { Pegando o tipo de EspecieDoc }
      if EspecieDoc = 'DM' then
         ATipoEspecieDoc   := '01'
      else if EspecieDoc = 'NP' then
         ATipoEspecieDoc   := '02'
      else if EspecieDoc = 'NS' then
         ATipoEspecieDoc   := '03'
      else if EspecieDoc = 'RC' then
         ATipoEspecieDoc   := '05'
      else if EspecieDoc = 'LC' then 
         ATipoEspecieDoc   := '08'
      else if EspecieDoc = 'DS' then
         ATipoEspecieDoc   := '12'
      else if EspecieDoc = 'ND' then
         ATipoEspecieDoc   := '13'; 

      { Pegando Tipo de Cobrança}
      case StrToInt(ACBrTitulo.Carteira) of
        11,17 :
          case CaracTitulo of
            tcSimples: aTipoCobranca:='     ';
            tcDescontada: aTipoCobranca:='04DSC';
            tcVendor: aTipoCobranca:='08VDR';
            tcVinculada: aTipoCobranca:='02VIN';
          else
            aTipoCobranca:='     ';
          end;
      else
        aTipoCobranca:='     ';
      end;

      {Intruções - Protesto}
      if (aTipoOcorrencia = '01') or (aTipoOcorrencia = '09') then
      begin
        if (DataProtesto > 0) and (DataProtesto > Vencimento) then
        begin
          AInstrucao   := '06'; // Protestar em xx dias corridos
          DiasProtesto := IntToStr(DaysBetween(DataProtesto, Vencimento))  
        end;        
      end
      else if ATipoOcorrencia = '02' then // 02-Pedido de Baixa      
        AInstrucao := '44';


      {Pegando Tipo de Sacado}
      case Sacado.Pessoa of
         pFisica   : ATipoSacado := '01';
         pJuridica : ATipoSacado := '02';
      else
         ATipoSacado := '00';
      end;

      {Pegando Tipo de Cedente}
      case ACBrBoleto.Cedente.TipoInscricao of
         pFisica   : ATipoCendente := '01';
         pJuridica : ATipoCendente := '02';
      end;

      AMensagem   := '';
      if Mensagem.Text <> '' then
         AMensagem   := Mensagem.Strings[0];


      with ACBrBoleto do
      begin
         wLinha:= '7' +                                                         // 001 a 001 - ID Registro
                  ATipoCendente +                                               // 002 a 003 - Tipo de inscrição da empresa 01-CPF / 02-CNPJ
                  PadLeft(OnlyNumber(Cedente.CNPJCPF),14,'0') +                 // 004 a 017 - Inscrição da empresa
                  aAgencia +                                                    // 018 a 021 - Prefixo da agência
                  PadRight( Cedente.AgenciaDigito, 1)  +                        // 022 a 022 - DV-prefixo da agencia
                  aConta +                                                      // 023 a 030 - Código do cendete/nr. conta corrente da empresa
                  PadRight( Cedente.ContaDigito, 1)  +                          // 031 a 031 - DV-código do cedente
                  PadLeft(OnlyNumber(Cedente.Convenio),7,'0') +                 // 032 a 038 - Número do convênio
                  PadRight( SeuNumero, 25 ) +                                   // 039 a 063 - Número de Controle do Participante
                  PadLeft( ANossoNumero, 17, '0') +                             // 064 a 080 - Nosso número
                  '0000' +                                                      // 081 a 084 - Zeros
                  '   ' +                                                       // 085 a 087 - Complemento do Registro: “Brancos”
                  ' ' +                                                         // 088 a 088 - Indic. Mensagem ou Sac.Avalista
                  '   ' +                                                       // 089 a 091 - Prefixo do título “Brancos”
                  aModalidade +                                                 // 092 a 094 - Variação da carteira
                  IntToStrZero(0,7) +                                           // 095 a 101 - Zero + Zeros
                  aTipoCobranca +                                               // 102 a 106 - Tipo de cobrança
                  Carteira +                                                    // 107 a 108 - Carteira
                  ATipoOcorrencia +                                             // 109 a 110 - Ocorrência "Comando"
                  PadRight( NumeroDocumento, 10, ' ') +                         // 111 a 120 - Seu Número - Nr. titulo dado pelo cedente
                  FormatDateTime( 'ddmmyy', Vencimento ) +                      // 121 a 126 - Data de vencimento
                  IntToStrZero( Round( ValorDocumento * 100 ), 13) +            // 127 a 139 - Valor do titulo
                  '085' + '0000' + ' ' +                                        // 140 a 147 - Numero do Banco - 085 + Prefixo da agência cobradora + DV-pref. agência cobradora
                  PadLeft(ATipoEspecieDoc, 2, '0') +                            // 148 a 149 - Especie de titulo
                  ATipoAceite +                                                 // 150 a 150 - Aceite
                  FormatDateTime( 'ddmmyy', DataDocumento ) +                   // 151 a 156 - Data de Emissão
                  PadLeft(AInstrucao, 4, '0') +                                 // 157 a 158 - Instrução codificada (cód. Protesto)
                  IntToStrZero( round(ValorMoraJuros * 100 ), 13) +             // 161 a 173 - Juros de mora por dia
                  IntToStrZero(0,6) +                                           // 174 a 179 - Zeros
                  IntToStrZero( round( ValorDesconto * 100), 13) +              // 180 a 192 - Valor do desconto
                  IntToStrZero(0,13) +                                          // 193 a 205 - Zeros
                  IntToStrZero( round( ValorAbatimento * 100 ), 13) +           // 206 a 218 - Valor do abatimento permitido
                  ATipoSacado +                                                 // 219 a 220 - Tipo de inscricao do sacado
                  PadLeft(OnlyNumber(Sacado.CNPJCPF),14,'0') +                  // 221 a 234 - CNPJ ou CPF do sacado
                  PadRight( Sacado.NomeSacado, 37) +                            // 235 a 271 - Nome do sacado
                  '   '  +                                                      // 272 a 274 - Brancos
                  PadRight(trim(Sacado.Logradouro) + ', ' +
                    trim(Sacado.Numero), 40) +                                  // 275 a 314 - Endereço do sacado
                  PadRight(trim(Sacado.Bairro), 12) +                           // 315 a 326 - Bairro do Sacado
                  PadLeft( OnlyNumber(Sacado.CEP), 8 ) +                        // 327 a 334 - CEP do endereço do sacado
                  PadRight( trim(Sacado.Cidade), 15) +                          // 335 a 349 - Cidade do sacado
                  PadRight( Sacado.UF, 2 ) +                                    // 350 a 351 - UF da cidade do sacado
                  PadRight( AMensagem, 40) +                                    // 352 a 391 - Observações
                  PadLeft(DiasProtesto,2,' ') +                                 // 392 a 393 - Número de dias para protesto (deixar em branco se não houver instrução de protesto)
                  ' ' +                                                         // 394 a 394 - Branco
                  IntToStrZero( aRemessa.Count + 1, 6 );                        // 395 a 400 - Sequencial de Registro 


         wLinha:= wLinha + sLineBreak +
                  '5' +                                                         //Tipo Registro
                  '99' +                                                        //Tipo de Serviço (Cobrança de Multa)
                  IfThen(PercentualMulta > 0, '2','9') +                        //Cod. Multa 2- Percentual 9-Sem Multa
                  IfThen(PercentualMulta > 0,
                         FormatDateTime('ddmmyy', DataMoraJuros),
                                        '000000') +                             //Data Multa
                  IntToStrZero( round( PercentualMulta * 100), 12) +            //Perc. Multa
                  Space(372) +                                                  //Brancos
                  IntToStrZero(aRemessa.Count + 2 ,6);

         aRemessa.Text := aRemessa.Text + UpperCase(wLinha);
      end;
   end;
end;

procedure TACBrBancoCecred.GerarRegistroTrailler400(
  ARemessa: TStringList);
var
  wLinha: String;
begin
   wLinha := '9' + Space(393)                     + // ID Registro
             IntToStrZero(ARemessa.Count + 1, 6);  // Contador de Registros

   ARemessa.Text:= ARemessa.Text + UpperCase(wLinha);
end;

function TACBrBancoCecred.TipoOCorrenciaToCod (
   const TipoOcorrencia: TACBrTipoOcorrencia ) : String;
begin
  case TipoOcorrencia of
    toRetornoRegistroConfirmado                         : Result := '02';
    toRetornoComandoRecusado                            : Result := '03';
    toRetornoLiquidadoSemRegistro                       : Result := '05';
    toRetornoLiquidado                                  : Result := '06';
    toRetornoLiquidadoPorConta                          : Result := '07';
//                                                      : Result := '08'; // 08-Liquidação por Saldo
    toRetornoBaixado                                    : Result := '09';
    toRetornoBaixaSolicitada                            : Result := '10';
    toRetornoTituloEmSer                                : Result := '11';
    toRetornoAbatimentoConcedido                        : Result := '12';
    toRetornoAbatimentoCancelado                        : Result := '13';
    toRetornoVencimentoAlterado                         : Result := '14';
    toRetornoLiquidadoEmCartorio                        : Result := '15';
//                                                      : Result := '16'; // 16-Confirmação de alteração de juros de mora
    toRetornoRecebimentoInstrucaoProtestar              : Result := '19';
    toRetornoDebitoEmConta                              : Result := '20';
    toRetornoRecebimentoInstrucaoAlterarNomeSacado      : Result := '21';
    toRetornoRecebimentoInstrucaoAlterarEnderecoSacado  : Result := '22';
    toRetornoEncaminhadoACartorio                       : Result := '23';
    toRetornoProtestoSustado                            : Result := '24';
    toRetornoJurosDispensados                           : Result := '25';
//                                                      : Result := '26'; // 26-Alteração do número do título dado pelo Cedente (Seu número) – 10 e 15 posições
    toRetornoManutencaoTituloVencido                    : Result := '28';
    toRetornoDescontoConcedido                          : Result := '31';
    toRetornoDescontoCancelado                          : Result := '32';
//                                                      : Result := '33'; // 33-Retificar desconto
//                                                      : Result := '34'; // 34-Alterar data para desconto
//                                                      : Result := '35'; // 35–Cobrar Multa
//                                                      : Result := '36'; // 36–Dispensar Multa
//                                                      : Result := '37'; // 37–Dispensar Indexador
//                                                      : Result := '38'; // 38–Dispensar prazo limite para recebimento
//                                                      : Result := '39'; // 39–Alterar prazo limite para recebimento
    toRetornoAcertoControleParticipante                 : Result := '41';
//                                                      : Result := '42'; // 42–Alteração do número do documento do sacado (CNPJ/CPF)
//                                                      : Result := '44'; // 44–Título pago com cheque devolvido
    toRetornoTituloPagoEmCheque                         : Result := '46';
    toRetornoTipoCobrancaAlterado                       : Result := '72';
    toRetornoDespesasProtesto                           : Result := '96';
    toRetornoDespesasSustacaoProtesto                   : Result := '97';
    toRetornoDebitoCustasAntecipadas                    : Result := '98';
  else
    Result := '02';
  end;
end;

function TACBrBancoCecred.TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String;
var
 CodOcorrencia: Integer;
begin

  CodOcorrencia := StrToIntDef(TipoOCorrenciaToCod(TipoOcorrencia),0);

  Case CodOcorrencia of
   {Segundo manual técnico CNAB400 Abril/2012 BB pag.20 os comandos são os seguintes:}
   02: Result:='02-Confirmação de Entrada de Título' ;
   03: Result:='03-Comando recusado' ;
   05: Result:='05-Liquidado sem registro' ;
   06: Result:='06-Liquidação Normal' ;
   07: Result:='07-Liquidação por Conta' ;
   08: Result:='08-Liquidação por Saldo' ;
   09: Result:='09-Baixa de Título' ;
   10: Result:='10-Baixa Solicitada' ;
   11: Result:='11-Titulos em Ser' ;
   12: Result:='12-Abatimento Concedido' ;
   13: Result:='13-Abatimento Cancelado' ;
   14: Result:='14-Alteração de Vencimento do Titulo' ;
   15: Result:='15-Liquidação em Cartório' ;
   16: Result:='16-Confirmação de alteração de juros de mora' ;
   19: Result:='19-Confirmação de recebimento de instruções para protesto' ;
   20: Result:='20-Débito em Conta' ;
   21: Result:='21-Alteração do Nome do Sacado' ;
   22: Result:='22-Alteração do Endereço do Sacado' ;
   23: Result:='23-Indicação de encaminhamento a cartório' ;
   24: Result:='24-Sustar Protesto' ;
   25: Result:='25-Dispensar Juros' ;
   26: Result:='26-Alteração do número do título dado pelo Cedente (Seu número) - 10 e 15 posições' ;
   28: Result:='28-Manutenção de titulo vencido' ;
   31: Result:='31-Conceder desconto' ;
   32: Result:='32-Não conceder desconto' ;
   33: Result:='33-Retificar desconto' ;
   34: Result:='34-Alterar data para desconto' ;
   35: Result:='35-Cobrar multa' ;
   36: Result:='36-Dispensar multa' ;
   37: Result:='37-Dispensar indexador' ;
   38: Result:='38-Dispensar prazo limite para recebimento' ;
   39: Result:='39-Alterar prazo limite para recebimento' ;
   41: Result:='41-Alteração do número do controle do participante (25 posições)' ;
   42: Result:='42-Alteração do número do documento do sacado (CNPJ/CPF)' ;
   44: Result:='44-Título pago com cheque devolvido' ;
   46: Result:='46-Título pago com cheque, aguardando compensação' ;
   72: Result:='72-Alteração de tipo de cobrança' ;
   96: Result:='96-Despesas de Protesto' ;
   97: Result:='97-Despesas de Sustação de Protesto' ;
   98: Result:='98-Débito de Custas Antecipadas' ;
  end;
end;

function TACBrBancoCecred.CodOcorrenciaToTipo(const CodOcorrencia:
   Integer ) : TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02: Result := toRetornoRegistroConfirmado;
    03: Result := toRetornoComandoRecusado;
    05: Result := toRetornoLiquidadoSemRegistro;
    06: Result := toRetornoLiquidado;
    07: Result := toRetornoLiquidadoPorConta;
    09: Result := toRetornoBaixado;
    10: Result := toRetornoBaixaSolicitada;
    11: Result := toRetornoTituloEmSer;
    12: Result := toRetornoAbatimentoConcedido;
    13: Result := toRetornoAbatimentoCancelado;
    14: Result := toRetornoVencimentoAlterado;
    15: Result := toRetornoLiquidadoEmCartorio;
    19: Result := toRetornoRecebimentoInstrucaoProtestar;
    21: Result := toRetornoRecebimentoInstrucaoAlterarNomeSacado;
    22: Result := toRetornoRecebimentoInstrucaoAlterarEnderecoSacado;
    23: Result := toRetornoEncaminhadoACartorio;
    24: Result := toRetornoProtestoSustado;
    25: Result := toRetornoJurosDispensados;
    28: Result := toRetornoManutencaoTituloVencido;
    31: Result := toRetornoDescontoConcedido;
    96: Result := toRetornoDespesasProtesto;
    97: Result := toRetornoDespesasSustacaoProtesto;
    98: Result := toRetornoDebitoCustasAntecipadas;
  else
    Result := toRetornoOutrasOcorrencias;
  end;
end;

function TACBrBancoCecred.CodMotivoRejeicaoToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: Integer): String;
begin
  case TipoOcorrencia of
    toRetornoComandoRecusado: //03 (Recusado)
      case CodMotivo of
        01: Result:='01-Identificação inválida' ;
        04: Result:='04-Valor do desconto inválido' ;
        05: Result:='05-Espécie de título inválida para carteira' ;
        08: Result:='08-Valor do título/apólice inválido' ;
        09: Result:='09-Data de vencimento inválida' ;
        18: Result:='18-Endereço do sacado não localizado ou incompleto' ;
        24: Result:='24-Valor do abatimento inválido' ;
        27: Result:='27-Nome do sacado/cedente inválido ou não informado' ;
        28: Result:='28-Data do novo vencimento inválida' ;
        30: Result:='30-Registro de título já liquidado' ;
        36: Result:='36-Dias para fichamento de protesto inválido' ;
        37: Result:='37-Data de emissão do título inválida' ;
        38: Result:='38-Data do vencimento anterior a data da emissão do título' ;
        39: Result:='39-Comando de alteração indevido para a carteira' ;
        41: Result:='41-Abatimento não permitido' ;
        42: Result:='42-CEP/UF inválido/não compatíveis (ECT)' ;
        52: Result:='52-Abatimento igual ou maior que o valor do título' ;
        68: Result:='68-Código/Data/Percentual de multa inválido' ;
        69: Result:='69-Valor/Percentual de juros inválido' ;
        80: Result:='80-Nosso número inválido' ;
        81: Result:='81-Data para concessão do desconto inválida' ;
        82: Result:='82-CEP do sacado inválido' ;
        84: Result:='84-Título não localizado na existencia' ;
        99: Result:='99-Outros motivos' ;
      end;

    toRetornoLiquidado,
    toRetornoLiquidadoEmCartorio:   // Comandos 06 e 15 posições 109/110 
      case CodMotivo of
        01: Result:='01-Liquidação normal';
        09: Result:='09-Liquidação em cartório';
      end;
    toRetornoRegistroConfirmado: //02 (Entrada)
      case CodMotivo of
        00: Result:='00-Por meio magnético';
        50: Result:='50-Sacado DDA';
      end;
    toRetornoBaixado, toRetornoBaixaSolicitada:  // 09 ou 10 (Baixa)
      case CodMotivo of
        00: Result:='00-Solicitada pelo cliente';
        15: Result:='15-Protestado';
        90: Result:='90-Baixa automática';
      end;
  end;
end;

procedure TACBrBancoCecred.LerRetorno240(ARetorno: TStringList);
var
  Titulo: TACBrTitulo;
  TempData, Linha, rCedente, rCNPJCPF: String;
  ContLinha : Integer;
  idxMotivo: Integer;
begin
   // informação do Header
   // Verifica se o arquivo pertence ao banco
   if StrToIntDef(copy(ARetorno.Strings[0], 1, 3),-1) <> Numero then
     raise Exception.create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                            'não' + 'é um arquivo de retorno do ' + Nome));

   ACBrBanco.ACBrBoleto.DataArquivo := StringToDateTimeDef(Copy(ARetorno[0],144,2)+'/'+
                                                           Copy(ARetorno[0],146,2)+'/'+
                                                           Copy(ARetorno[0],148,4),0, 'DD/MM/YYYY' );

   ACBrBanco.ACBrBoleto.NumeroArquivo := StrToIntDef(Copy(ARetorno[0],158,6),0);

   rCedente := trim(copy(ARetorno[0], 73, 30));
   rCNPJCPF := OnlyNumber( copy(ARetorno[0], 19, 14) );

   with ACBrBanco.ACBrBoleto do
   begin
     if (not LeCedenteRetorno) and (rCNPJCPF <> OnlyNumber(Cedente.CNPJCPF)) then
       raise Exception.create(ACBrStr('CNPJ\CPF do arquivo inválido'));

     if LeCedenteRetorno then
     begin
       Cedente.Nome    := rCedente;
       Cedente.CNPJCPF := rCNPJCPF;

       if StrToIntDef(copy(ARetorno[0], 18, 1), 0) = 1 then
         Cedente.TipoInscricao := pFisica
       else
         Cedente.TipoInscricao := pJuridica;
     end;

     ACBrBanco.ACBrBoleto.ListadeBoletos.Clear;
   end;

   ACBrBanco.TamanhoMaximoNossoNum := 20;

   Linha := '';
   Titulo := nil;

   for ContLinha := 1 to ARetorno.Count - 2 do
   begin
     Linha := ARetorno[ContLinha];

     if copy(Linha, 8, 1) <> '3' then // verifica se o registro (linha) é um registro detalhe (segmento J)
       Continue;

     if copy(Linha, 14, 1) = 'T' then // se for segmento T cria um novo titulo
       Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

     if Assigned(Titulo) then
     with Titulo do
     begin
       if copy(Linha, 14, 1) = 'T' then
       begin
         SeuNumero       := copy(Linha, 106, 25);
         NumeroDocumento := copy(Linha, 59, 15);
         Carteira        := copy(Linha, 58, 1);

         TempData := copy(Linha, 74, 2) + '/'+copy(Linha, 76, 2)+'/'+copy(Linha, 78, 4);
         if TempData<>'00/00/0000' then
           Vencimento := StringToDateTimeDef(TempData, 0, 'DDMMYY');

         ValorDocumento       := StrToFloatDef(copy(Linha, 82, 15), 0) / 100;
         NossoNumero          := copy(Linha, 38, 20);
         ValorDespesaCobranca := StrToFloatDef(copy(Linha, 199, 15), 0) / 100;

         OcorrenciaOriginal.Tipo := CodOcorrenciaToTipo(StrToIntDef(copy(Linha, 16, 2), 0));

         IdxMotivo := 214;

         while (IdxMotivo < 223) do
         begin
           if (trim(Copy(Linha, IdxMotivo, 2)) <> '') then
           begin
             MotivoRejeicaoComando.Add(Copy(Linha, IdxMotivo, 2));
             DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo, StrToIntDef(Copy(Linha, IdxMotivo, 2), 0)));
           end;
           Inc(IdxMotivo, 2);
         end;
       end
       else // segmento U
       begin
         ValorIOF            := StrToFloatDef(copy(Linha, 63, 15), 0) / 100;
         ValorAbatimento     := StrToFloatDef(copy(Linha, 48, 15), 0) / 100;
         ValorDesconto       := StrToFloatDef(copy(Linha, 33, 15), 0) / 100;
         ValorMoraJuros      := StrToFloatDef(copy(Linha, 18, 15), 0) / 100;
         ValorOutrosCreditos := StrToFloatDef(copy(Linha, 108, 15), 0) / 100;
         ValorRecebido       := StrToFloatDef(copy(Linha, 78, 15), 0) / 100;

         TempData := copy(Linha, 138, 2)+'/'+copy(Linha, 140, 2)+'/'+copy(Linha, 142, 4);
         if TempData <> '00/00/0000' then
           DataOcorrencia := StringToDateTimeDef(TempData, 0, 'DDMMYY');

         TempData := copy(Linha, 146, 2)+'/'+copy(Linha, 148, 2)+'/'+copy(Linha, 150, 4);
         if TempData<>'00/00/0000' then
           DataCredito := StringToDateTimeDef(TempData, 0, 'DDMMYYYY');
       end;
     end;
   end;

   ACBrBanco.TamanhoMaximoNossoNum := 10;
end;

procedure TACBrBancoCecred.LerRetorno400(ARetorno: TStringList);
var
  Titulo : TACBrTitulo;
  ContLinha, CodOcorrencia, CodMotivo, MotivoLinha : Integer;
  rAgencia, rDigitoAgencia, rConta :String;
  rDigitoConta, rCodigoCedente     :String;
  Linha, rCedente                  :String;
begin
   fpTamanhoMaximoNossoNum := 17;

   if StrToIntDef(copy(ARetorno.Strings[0],77,3),-1) <> Numero then
      raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                             'não é um arquivo de retorno do '+ Nome));

   rCedente      := trim(Copy(ARetorno[0],47,30));
   rAgencia      := trim(Copy(ARetorno[0],27,4));
   rDigitoAgencia:= Copy(ARetorno[0],31,1);
   rConta        := trim(Copy(ARetorno[0],32,8));
   rDigitoConta  := Copy(ARetorno[0],40,1);

   rCodigoCedente:= Copy(ARetorno[0],41,6);


   ACBrBanco.ACBrBoleto.NumeroArquivo := StrToIntDef(Copy(ARetorno[0],41,6),0);

   ACBrBanco.ACBrBoleto.DataArquivo   := StringToDateTimeDef(Copy(ARetorno[0],95,2)+'/'+
                                                             Copy(ARetorno[0],97,2)+'/'+
                                                             Copy(ARetorno[0],99,2),0, 'DD/MM/YY' );

   with ACBrBanco.ACBrBoleto do
   begin
      if (not LeCedenteRetorno) and
         ((rAgencia <> OnlyNumber(Cedente.Agencia)) or
          (StrToIntDef(rConta,0) <> StrToIntDef(OnlyNumber(Cedente.Conta),0))) then
         raise Exception.Create(ACBrStr('Agência\Conta do arquivo inválido'));

      Cedente.Nome         := rCedente;
      Cedente.Agencia      := rAgencia;
      Cedente.AgenciaDigito:= rDigitoAgencia;
      Cedente.Conta        := rConta;
      Cedente.ContaDigito  := rDigitoConta;
      Cedente.CodigoCedente:= rCodigoCedente;

      ACBrBanco.ACBrBoleto.ListadeBoletos.Clear;
   end;

   ACBrBanco.TamanhoMaximoNossoNum := fpTamanhoMaximoNossoNum;

   for ContLinha := 1 to ARetorno.Count - 2 do
   begin
      Linha := ARetorno[ContLinha] ;

      if (Copy(Linha,1,1) <> '7') then
         Continue;

      Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

      with Titulo do
      begin
         SeuNumero                   := copy(Linha,38,25);
         NumeroDocumento             := copy(Linha,117,10);
         OcorrenciaOriginal.Tipo     := CodOcorrenciaToTipo(StrToIntDef(
                                        copy(Linha,109,2),0));

         CodOcorrencia := StrToInt(IfThen(copy(Linha,109,2) = '00','00',copy(Linha,109,2)));

         if (CodOcorrencia = 5) or
            (CodOcorrencia = 6) or
            (CodOcorrencia = 7) or
            (CodOcorrencia = 8) or
            (CodOcorrencia = 15) or
            (CodOcorrencia = 46) then
         begin
           CodigoLiquidacao := copy(Linha,109,2);
           CodigoLiquidacaoDescricao := TipoOcorrenciaToDescricao(OcorrenciaOriginal.Tipo);
         end;


         if(CodOcorrencia >= 2) and ((CodOcorrencia <= 10)) then
         begin
           MotivoLinha:= 81;
           CodMotivo:= StrToInt(IfThen(copy(Linha,MotivoLinha,2) = '00','00',copy(Linha,MotivoLinha,2)));
           MotivoRejeicaoComando.Add(copy(Linha,MotivoLinha,2));
           DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo,CodMotivo));
         end;

         DataOcorrencia := StringToDateTimeDef( Copy(Linha,111,2)+'/'+
                                                Copy(Linha,113,2)+'/'+
                                                Copy(Linha,115,2),0, 'DD/MM/YY' );

         Vencimento := StringToDateTimeDef( Copy(Linha,147,2)+'/'+
                                            Copy(Linha,149,2)+'/'+
                                            Copy(Linha,151,2),0, 'DD/MM/YY' );

         ValorDocumento       := StrToFloatDef(Copy(Linha,153,13),0)/100;
         ValorIOF             := StrToFloatDef(Copy(Linha,215,13),0)/100;
         ValorAbatimento      := StrToFloatDef(Copy(Linha,228,13),0)/100;
         ValorDesconto        := StrToFloatDef(Copy(Linha,241,13),0)/100;
         ValorRecebido        := StrToFloatDef(Copy(Linha,306,13),0)/100;
         ValorMoraJuros       := StrToFloatDef(Copy(Linha,267,13),0)/100;
         ValorOutrosCreditos  := StrToFloatDef(Copy(Linha,280,13),0)/100;
         NossoNumero          := Copy(Linha,64,17);
         Carteira             := Copy(Linha,107,2);
         ValorDespesaCobranca := StrToFloatDef(Copy(Linha,182,07),0)/100;
         ValorOutrasDespesas  := StrToFloatDef(Copy(Linha,189,13),0)/100;

         if StrToIntDef(Copy(Linha,176,6),0) <> 0 then
            DataCredito:= StringToDateTimeDef( Copy(Linha,176,2)+'/'+
                                               Copy(Linha,178,2)+'/'+
                                               Copy(Linha,180,2),0, 'DD/MM/YY' );
      end;
   end;

   fpTamanhoMaximoNossoNum := 17;
end;


end.
