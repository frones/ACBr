
{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: DOUGLAS TYBEL dtybel@yahoo.com.br               }
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

unit ACBrBancoBancoob;

interface

uses
  Classes, SysUtils, ACBrBoleto, ACBrBoletoConversao, ACBrUtil.Base;

type

  { TACBrBancoob}

  TACBrBancoob = class(TACBrBancoClass)
   protected
   private
      FNumeroSequencialRegistroNoLote: Int64;
      fpValorTotalDocs: Double;
   public
    Constructor create(AOwner: TACBrBanco);
    function CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo ): String; override;
    function MontarCodigoBarras(const ACBrTitulo : TACBrTitulo): String; override;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String; override;
    function MontarCampoNossoNumero(const ACBrTitulo :TACBrTitulo): String; override;
    procedure GerarRegistroHeader400(NumeroRemessa : Integer; aRemessa: TStringList); override;
    procedure GerarRegistroTransacao400(ACBrTitulo :TACBrTitulo; aRemessa: TStringList); override;
    procedure GerarRegistroTrailler400( ARemessa: TStringList  ); override;
    function GerarRegistroHeader240(NumeroRemessa : Integer): String; override;
    function GerarRegistroTransacao240(ACBrTitulo : TACBrTitulo): String; override;
    function GerarRegistroTrailler240(ARemessa : TStringList): String;  override;
    Procedure LerRetorno400(ARetorno:TStringList); override;
    procedure LerRetorno240(ARetorno: TStringList); override;
    function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia) : String; override;
    function CodOcorrenciaToTipo(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
    function TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia):String; override;
    function CodMotivoRejeicaoToDescricao(const TipoOcorrencia:TACBrTipoOcorrencia; CodMotivo:Integer): String; override;

    function CodOcorrenciaToTipoRemessa(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
   end;

implementation

uses  StrUtils, Variants, math,
      {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF},
      ACBrUtil.FilesIO, ACBrUtil.Strings, ACBrUtil.DateTime;

constructor TACBrBancoob.create(AOwner: TACBrBanco);
begin
   inherited create(AOwner);
   fpDigito := 0;
   fpNome   := 'SICOOB';
   fpNumero := 756;
   fpTamanhoMaximoNossoNum := 7;
   fpTamanhoCarteira   := 1;
   fpTamanhoConta      := 12;
   fpCodigosMoraAceitos:= '012';
   fpLayoutVersaoArquivo := 81;
   fpLayoutVersaoLote    := 40;
   fpValorTotalDocs := 0;
end;

function TACBrBancoob.CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo ): String;
var
  Num, Res :String;
  i, base, digito : Integer;
const
  indice = '319731973197319731973';
begin

   Result := '0';

   Num :=  PadLeft(ACBrTitulo.ACBrBoleto.Cedente.Agencia, 4, '0') +
           PadLeft(ACBrTitulo.ACBrBoleto.Cedente.CodigoCedente, 10, '0') +
           PadLeft(trim(ACBrTitulo.NossoNumero), 7, '0');


   base := 0;
   for i := 1 to Length(Num) do
     base := base + ( StrToInt(copy(Num,i,1)) * StrToInt(copy(indice,i,1)) );

   digito := 11-((  base )-( trunc(base/11) * 11));
   //(Se o Resto for igual a 0 ou 1 então o DV é igual a 0)
   if (digito > 9) then
      digito := 0;

   Res    := IntToStr(digito);
   Result := Res;

   { Para o cálculo do dígito verificador do nosso número, deverá ser utilizada
     a fórmula abaixo:
     Número da Cooperativa    9(4) – 3009
     Código do Cliente   9(10) – cedente
     Nosso Número   9(7) – Iniciado contagem em 1

     Constante para cálculo  = 3197


     a) Concatenar na seqüência completando com zero à esquerda.
        Ex.: Número da Cooperativa  = 0001
             Número do Cliente(cedente)  = 1-9
             Nosso Número  = 21
             000100000000190000021

     b) Alinhar a constante com a seqüência repetindo de traz para frente.
        Ex.: 000100000000190000021
             319731973197319731973

     c) Multiplicar cada componente da seqüência com o seu correspondente da
        constante e somar os resultados.
        Ex.: 1*7 + 1*3 + 9*1 + 2*7 + 1*3 = 36

     d) Calcular o Resto através do Módulo 11.
        Ex.: 36/11 = 3, resto = 3

     e) O resto da divisão deverá ser subtraído de 11 achando assim o DV
        (Se o Resto for igual a 0 ou 1 então o DV é igual a 0).
        Ex.: 11 – 3 = 8, então Nosso Número + DV = 21-8


     Memória de Cálculo
     Coop.(4)|Cliente(10)		    |Nosso Número(7)
     3	   0	 0	9	0	0	0	0	1	3	6	3	5	2	5	9	3	1	1	5	1
     3	   1 	 9	7	3	1	9	7	3	1	9	7	3	1	9	7	3	1	9	7	3
     9	   0	 0	63	0	0	0	0	3	3	54	21	15	2	45	63	9	1	9	35	3 = soma = 335

     digito = 11-((  soma )-( resto inteiro (trunc) da divisao da soma por 11 * 11))
     digito = 11-((  335 )-(30*11))
     digito = 6 }
end;

function TACBrBancoob.MontarCodigoBarras(const ACBrTitulo : TACBrTitulo): String;
var
  CodigoBarras, FatorVencimento, DigitoCodBarras, ANossoNumero,ACarteira :String;
  CampoLivre : String;
begin

    FatorVencimento := CalcularFatorVencimento(ACBrTitulo.Vencimento);
    ANossoNumero := ACBrTitulo.NossoNumero+CalcularDigitoVerificador(ACBrTitulo);

    if (ACBrTitulo.Carteira = '1') or (ACBrTitulo.Carteira = '3')then
       ACarteira := ACBrTitulo.Carteira
    else
       raise Exception.Create( ACBrStr('Carteira Inválida.'+sLineBreak+'Utilize "1" ou "3".') );

    {Montando Campo Livre}
    CampoLivre    := PadLeft(trim(ACBrTitulo.ACBrBoleto.Cedente.Modalidade), 2, '0') +
                     PadLeft(trim(ACBrTitulo.ACBrBoleto.Cedente.CodigoCedente), 7, '0') +
                     PadLeft(Copy(ANossoNumero,1,8), 8, '0') +  //7 Sequenciais + 1 do digito
                     IntToStrZero(Max(1,ACBrTitulo.Parcela),3);

    {Codigo de Barras}
    with ACBrTitulo.ACBrBoleto do
    begin
       CodigoBarras := IntToStrZero(Banco.Numero, 3) +
                       '9' +
                       FatorVencimento +
                       IntToStrZero(Round(ACBrTitulo.ValorDocumento * 100), 10) +
                       PadLeft(ACarteira, 1, '0') +
                       PadLeft(OnlyNumber(Cedente.Agencia),4,'0') +
                       CampoLivre;
    end;

    DigitoCodBarras := CalcularDigitoCodigoBarras(CodigoBarras);
    Result:= copy( CodigoBarras, 1, 4) + DigitoCodBarras + copy( CodigoBarras, 5, 44);
end;

function TACBrBancoob.MontarCampoCodigoCedente (
   const ACBrTitulo: TACBrTitulo ) : String;

var
  CodigoCedente: String;
begin
  CodigoCedente := ACBrTitulo.ACBrBoleto.Cedente.CodigoCedente;
  Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia + '/'+
            copy(CodigoCedente,1,length(CodigoCedente)-1)+ '-'+
            copy(CodigoCedente,length(CodigoCedente),1);
end;

function TACBrBancoob.MontarCampoNossoNumero (const ACBrTitulo: TACBrTitulo ) : String;
begin
  Result := ACBrTitulo.NossoNumero + '-' + CalcularDigitoVerificador(ACBrTitulo);
end;

procedure TACBrBancoob.GerarRegistroHeader400(NumeroRemessa : Integer; aRemessa:TStringList);
var
  wLinha: String;
begin
   with ACBrBanco.ACBrBoleto.Cedente do
   begin
      wLinha:= '0'                                        + // ID do Registro
               '1'                                        + // ID do Arquivo( 1 - Remessa)
               'REMESSA'                                  + // Literal de Remessa
               '01'                                       + // Código do Tipo de Serviço
               PadRight( 'COBRANÇA', 8 )                      + // Descrição do tipo de serviço
               Space(7)                                   + // Brancos
               PadLeft( OnlyNumber(Agencia), 4 )             + // Prefixo da Cooperativa
               PadLeft( AgenciaDigito, 1 )                   + // Dígito Verificador do Prefixo
               PadLeft( trim(CodigoCedente), 9,'0' )         + // Código do Cliente/Cedente
               Space(6)                                   + // Brancos
               PadLeft( Nome, 30 )                           + // Nome do Cedente
               PadRight( '756BANCOOBCED', 18 )                + // Identificação do Banco: "756BANCOOBCED"  //Enviado pelo pessoal da homologação por email
               FormatDateTime('ddmmyy',Now)               + // Data de geração do arquivo
               IntToStrZero(NumeroRemessa,7)              + // Seqüencial da Remessa: número seqüencial acrescido de 1 a cada remessa. Inicia com "0000001"
               Space(287)                                 + // Brancos
               IntToStrZero(1,6);                           // Contador de Registros

      aRemessa.Text:= aRemessa.Text + UpperCase(wLinha);
   end;
end;

procedure TACBrBancoob.GerarRegistroTransacao400(ACBrTitulo :TACBrTitulo; aRemessa: TStringList);
var
  DigitoNossoNumero, Ocorrencia,aEspecie, AInstrucao1, AInstrucao2 :String;
  TipoSacado, ATipoAceite,MensagemCedente, DiasProtesto :String;
  TipoCedente, wLinha :String;
  I: Integer;
  wRespEntrega: Char;
  strDataDesconto, strValorDesconto:String;
  strCarteiraEnvio : Char;
begin

    if (Length(ACBrTitulo.Carteira) < 1 )then
       raise Exception.Create( ACBrStr('Carteira Inválida.'+sLineBreak) ) ;

   with ACBrTitulo do
   begin
      DigitoNossoNumero := CalcularDigitoVerificador(ACBrTitulo);

      {Pegando Código da Ocorrencia}
      case OcorrenciaOriginal.Tipo of
         toRemessaBaixar                         : Ocorrencia := '02'; {Pedido de Baixa}
         toRemessaConcederAbatimento             : Ocorrencia := '04'; {Concessão de Abatimento}
         toRemessaCancelarAbatimento             : Ocorrencia := '05'; {Cancelamento de Abatimento concedido}
         toRemessaAlterarVencimento              : Ocorrencia := '06'; {Alteração de vencimento}
         toRemessaAlterarNumeroControle          : Ocorrencia := '08'; {Alteração de seu número}
         toRemessaProtestar                      : Ocorrencia := '09'; {Pedido de protesto}
         toRemessaCancelarInstrucaoProtestoBaixa : Ocorrencia := '10'; {Desistência do Protesto e Baixar Título}
         toRemessaDispensarJuros                 : Ocorrencia := '11'; {Instrução para Dispensar Juros}
         toRemessaAlterarDadosPagador            : Ocorrencia := '12'; {Alteração de Pagador}
         toRemessaOutrasOcorrencias,
           toRemessaAlterarOutrosDados           : Ocorrencia := '31'; {Alteração de Outros Dados}

         toRemessaBaixaporPagtoDiretoCedente     : Ocorrencia := '34'; {Baixa - Pagamento Direto ao Beneficiário}
      else
         Ocorrencia := '01';                                          {Remessa}
      end;

      { Pegando o Aceite do Titulo }
      case Aceite of
         atSim :  ATipoAceite := '1';
         atNao :  ATipoAceite := '0';
      end;

      {INstrucao}
      AInstrucao1 := PadLeft(Trim(Instrucao1),2,'0');
      AInstrucao2 := PadLeft(Trim(Instrucao2),2,'0');

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

      {Pegando Tipo de Sacado}
      case Sacado.Pessoa of
         pFisica   : TipoSacado := '01';
         pJuridica : TipoSacado := '02';
      else
         TipoSacado := '99';
      end;

      {Pegando Tipo de Cedente}
      if ACBrBoleto.Cedente.TipoInscricao  = pFisica then
         TipoCedente := '01'
      else
         TipoCedente := '02';

      if ACBrBoleto.Cedente.ResponEmissao= tbCliEmite then
         wRespEntrega := '2'
      else
         wRespEntrega := '1';
      
      if (ACBrTitulo.CarteiraEnvio = tceCedente) then
        strCarteiraEnvio := '2'
      else
        strCarteiraEnvio := '1';

      DiasProtesto := IntToStrZero(DiasDeProtesto,2);
         
      { Data do Primeiro Desconto}
      if ( DataDesconto <> 0 ) then
        strDataDesconto := FormatDateTime('ddmmyy', DataDesconto)
      else
        strDataDesconto := IntToStrZero(0, 6);

      { Valor do Primeiro Desconto}
      if ( ValorDesconto <> 0 ) then
        strValorDesconto := IntToStrZero( Round( ValorDesconto * 100 ), 13)
      else
        strValorDesconto := IntToStrZero(0, 13);

      with ACBrBoleto do
      begin
         MensagemCedente:= '';
         for I:= 0 to Mensagem.count-1 do
             MensagemCedente:= MensagemCedente + trim(Mensagem[i]);

         if length(MensagemCedente) > 40 then
            MensagemCedente:= copy(MensagemCedente,1,40);

         wLinha:= '1'                                                     +  // ID Registro
                  TipoCedente                                             +  // Identificação do Tipo de Inscrição do Sacado 01 - CPF 02 - CNPJ
                  PadLeft(onlyNumber(Cedente.CNPJCPF),14,' ')             +  // Número de Inscrição do Cedente
                  PadLeft(OnlyNumber(Cedente.Agencia), 4, '0')            +  // Agência
                  PadLeft( Cedente.AgenciaDigito, 1, '0')                 +  // Agência digito
                  PadLeft( RightStr(OnlyNumber(Cedente.Conta),8), 8, '0') +  // Conta Corrente
                  PadLeft( Cedente.ContaDigito, 1, ' ')                   +  // Dígito Conta Corrente
                  PadLeft( '0', 6, '0')                                   +  // Número do Convênio de Cobrança do Cedente fixo zeros: "000000"
                  PadRight(trim(SeuNumero),  25)                          +  // Seu Numero (antes etava indo Brancos)
                  PadLeft( NossoNumero + DigitoNossoNumero, 12, '0')      +  // Nosso Número + //nosso numero com digito
                  IntToStrZero(ifthen(Parcela > 0, Parcela,1),2)          +  // Número da Parcela: "01" se parcela única
                  '00'                                                    +  // Grupo de Valor: "00"
                  Space(3)                                                +  // Brancos
                  Space(1)                                                +  // Indicativo de Mensagem ou Sacador/Avalista:
                  Space(3)                                                +  // Brancos
                  IntToStrZero( 0, 3)                                     +  // Variação da Carteira: "000"
                  IntToStrZero( 0, 1)                                     +  // Conta Caução: "0"
                  IntToStrZero( 0, 5)                                     +  // Código de responsabilidade: "00000"
                  IntToStrZero( 0, 1)                                     +  // DV do código de responsabilidade: "0"
                  IntToStrZero( 0, 6)                                     +  // Numero do borderô: “000000”
                  Space(4)                                                +  // Brancos
                  wRespEntrega                                            +  // Tipo de Emissão 1-Cooperativa - 2-Cliente
                  PadLeft( trim(Cedente.Modalidade), 2, '0')              +  // Carteira/Modalidade
                  Ocorrencia                                              +  // Ocorrencia (remessa)
                  PadRight(trim(NumeroDocumento),  10)                    +  // Número do Documento
                  FormatDateTime( 'ddmmyy', Vencimento)                   +  // Data de Vencimento do Título
                  IntToStrZero( Round( ValorDocumento * 100 ), 13)        +  // Valor do Título
                  IntToStrZero( Banco.Numero, 3)                          +  // Número Banco: "756"
                  PadLeft(OnlyNumber(Cedente.Agencia), 4, '0')            +  // Prefixo da Agência Cobradora: “0000”
                  PadLeft( Cedente.AgenciaDigito, 1, ' ')                 +  // Dígito Verificador do Prefixo da Agência Cobradora: Brancos
                  PadRight(aEspecie,2)                                    +  // Espécie do Título
                  ATipoAceite                                             +  // Identificação
                  FormatDateTime( 'ddmmyy', DataDocumento )               +  // 32 Data de Emissão
                  PadLeft(AInstrucao1, 2, '0')                            +  // 33 Primeira instrução (SEQ 34) = 00 e segunda (SEQ 35) = 00, não imprime nada.
                  PadLeft(AInstrucao2, 2, '0')                            +  // 34 Primeira instrução (SEQ 34) = 00 e segunda (SEQ 35) = 00, não imprime nada.
                  IntToStrZero( Round( (ValorMoraJuros) * 10000 ), 6)     +  // Taxa de mora mês
                  IntToStrZero( Round( PercentualMulta * 10000 ), 6)      +  // Taxa de multa
                  strCarteiraEnvio                                        +  // Responsabilidade Distribuição
                  strDataDesconto                                         +  // Data do Primeiro Desconto, Preencher com zeros quando não for concedido nenhum desconto.
                  strValorDesconto                                        +  // Valor do Primeiro Desconto, Preencher com zeros quando não for concedido nenhum desconto.
                  IntToStrZero( 9 , 1)                                    +  // MOEDA 9 BRASIL
                  IntToStrZero( 0, 12)                                    +  // Valor IOF / Quantidade Monetária: "0000000000000"
                  IntToStrZero( 0, 13)                                    +  // Valor Abatimento
                  TipoSacado                                              +  // Tipo de Inscrição do Sacado: 01 - CPF 02 - CNPJ
                  PadLeft(onlyNumber(Sacado.CNPJCPF),14,'0')              +  // Número de Inscrição do Sacado
                  PadRight( Sacado.NomeSacado, 40, ' ')                   +  // Nome do Sacado
                  PadRight( Sacado.Logradouro +' '+ Sacado.Numero,37,' ') +  // Endereço Completo
                  PadRight( Sacado.Bairro,15,' ')                         +  // Endereço Bairro
                  PadRight( Sacado.CEP,8,' ')                             +  // Endereço CEP
                  PadRight( Sacado.Cidade,15,' ')                         +  // Endereço cidade
                  PadRight( Sacado.UF,2,' ')                              +  // Endereço uf
                  PadRight( trim(MensagemCedente) ,40,' ')                +  // Observações/Mensagem ou Sacador/Avalista:
                  DiasProtesto                                            +  // Número de Dias Para Protesto
                  Space(1)                                                +  // Brancos
                  IntToStrZero( aRemessa.Count + 1, 6 );                     // Contador de Registros;

         aRemessa.Text:= aRemessa.Text + UpperCase(wLinha);
      end;
   end;
end;

procedure TACBrBancoob.GerarRegistroTrailler400( ARemessa: TStringList );
var
  wLinha: String;
begin
   wLinha:= '9'                                  + // ID Registro
            Space(193)                           + // Brancos
            Space(40)                            + // Mensagem responsabilidade Cedente
            Space(40)                            + // Mensagem responsabilidade Cedente
            Space(40)                            +
            Space(40)                            +
            Space(40)                            +
            IntToStrZero( ARemessa.Count + 1, 6);  // Contador de Registros

   ARemessa.Text:= ARemessa.Text + UpperCase(wLinha);
end;


procedure TACBrBancoob.LerRetorno240(ARetorno: TStringList);
var
  ContLinha: Integer;
  Titulo   : TACBrTitulo;
  Linha, rCedente, rCNPJCPF: String;
  rAgencia, rConta,rDigitoConta: String;
  MotivoLinha, I, CodMotivo: Integer;
  codOcorrencia: String;
begin
 
   if (copy(ARetorno.Strings[0],1,3) <> '756') then
      raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                             'não é um arquivo de retorno do '+ Nome));

   with ACBrBanco.ACBrBoleto do
   begin
     if LeCedenteRetorno then
     begin
       case StrToIntDef(Copy(ARetorno[1],18,1),0) of
         1: Cedente.TipoInscricao:= pFisica;
         2: Cedente.TipoInscricao:= pJuridica;
       else
         Cedente.TipoInscricao:= pJuridica;
       end;
     end;

     rCedente := trim(Copy(ARetorno[0],73,30));
     rAgencia := trim(Copy(ARetorno[0],53,5));
     rConta   := trim(Copy(ARetorno[0],59,12));
     rDigitoConta := Copy(ARetorno[0],71,1);

     NumeroArquivo := StrToIntDef(Copy(ARetorno[0], 158, 6), 0);
     DataArquivo   := StringToDateTimeDef(Copy(ARetorno[0],144,2)+'/'+
                                          Copy(ARetorno[0],146,2)+'/'+
                                          Copy(ARetorno[0],148,4),0, 'DD/MM/YYYY' );

     if StrToIntDef(Copy(ARetorno[1],200,6),0) <> 0 then
        DataCreditoLanc := StringToDateTimeDef(Copy(ARetorno[0],200,2)+'/'+
                                               Copy(ARetorno[0],202,2)+'/'+
                                               Copy(ARetorno[0],204,4),0, 'DD/MM/YYYY' );
     rCNPJCPF := trim( Copy(ARetorno[0],19,14)) ;

     if Cedente.TipoInscricao = pJuridica then
     begin
       rCNPJCPF := trim( Copy(ARetorno[1],19,15));
       rCNPJCPF := RightStr(rCNPJCPF,14) ;
     end
     else
     begin
       rCNPJCPF := trim( Copy(ARetorno[1],23,11));
       rCNPJCPF := RightStr(rCNPJCPF,11) ;
     end;


     if ( (not LeCedenteRetorno) and (rCNPJCPF <> OnlyNumber(Cedente.CNPJCPF)) ) then
       raise Exception.Create(ACBrStr('CNPJ\CPF do arquivo inválido'));

     if ( (not LeCedenteRetorno) and (StrToInt(rAgencia) <> StrToInt(Cedente.Agencia)) ) then
       raise Exception.CreateFMT('Agencia do arquivo %s inválida, config %s',[rAgencia,OnlyNumber(Cedente.Agencia)]);

     if ( (not LeCedenteRetorno) and (rConta + rDigitoConta <> OnlyNumber(Cedente.Conta + Cedente.ContaDigito)) ) then
       raise Exception.CreateFMT('Conta do arquivo %s inválida, config %s',[rConta,OnlyNumber(Cedente.Conta + Cedente.ContaDigito)]);

     if LeCedenteRetorno then
     begin
       Cedente.Nome    := rCedente;
       Cedente.CNPJCPF := rCNPJCPF;
       Cedente.Agencia := rAgencia;
       Cedente.AgenciaDigito:= '0';
       Cedente.Conta   := rConta;
       Cedente.ContaDigito:= rDigitoConta;
       Cedente.CodigoCedente:= rConta+rDigitoConta;
     end;
     Cedente.Conta := RemoveZerosEsquerda(Cedente.Conta);

     ListadeBoletos.Clear;
   end;

   Linha := '';
   Titulo := nil;

   for ContLinha := 1 to ARetorno.Count - 2 do
   begin
      Linha := ARetorno[ContLinha] ;

      {Segmento T - Só cria após passar pelo seguimento T depois U}
      if Copy(Linha,14,1)= 'T' then
         Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

      if Assigned(Titulo) then
      with Titulo do
      begin
         {Segmento T}
         if Copy(Linha,14,1)= 'T' then
          begin
            SeuNumero                   := Trim(copy(Linha,106,25));
            NumeroDocumento             := copy(Linha,59,15);
            codOcorrencia               := copy(Linha,16,2);
            OcorrenciaOriginal.Tipo     := CodOcorrenciaToTipo(StrToIntDef(codOcorrencia,0));

            //05 = Liquidação Sem Registro
            Vencimento := StringToDateTimeDef( Copy(Linha,74,2)+'/'+
                                               Copy(Linha,76,2)+'/'+
                                               Copy(Linha,80,2),0, 'DD/MM/YY' );

            ValorDocumento       := StrToFloatDef(Copy(Linha,82,15),0)/100;
            ValorDespesaCobranca := StrToFloatDef(Copy(Linha,199,15),0)/100;
            NossoNumero          := Copy(Linha,40,7);
            Carteira             := Copy(Linha,58,1);

            if (CodOcorrencia  = '06' ) or (CodOcorrencia  = '09' ) or
               (CodOcorrencia  = '17' ) then
            begin
              CodigoLiquidacao     := Copy(Linha,214,02);
            //CodigoLiquidacaoDescricao := CodigoLiquidacao_Descricao( StrToIntDef(CodigoLiquidacao,0) );
            end;
            
            if (Copy(Linha,133,1) = '1') then
             begin 
              Sacado.Pessoa  := pFisica;
	      Sacado.CNPJCPF := Copy(Linha,138,11);
             end
            else if (Copy(Linha,133,1) = '2') then
             begin 
              Sacado.Pessoa := pJuridica;
              Sacado.CNPJCPF := Copy(Linha,135,14);
             end
            else
              Sacado.Pessoa := pOutras;

            Sacado.NomeSacado := Trim(Copy(Linha,149,40));

            // DONE -oJacinto Junior: Implementar a leitura dos motivos das ocorrências.
            MotivoLinha := 214;

            for I := 0 to 4 do
            begin
              CodMotivo := StrToIntDef(IfThen(Copy(Linha, MotivoLinha, 2) = ACBrUtil.Strings.Poem_Zeros('0', 2), '00', Copy(Linha, MotivoLinha, 2)), 0);

              if CodMotivo <> 0 then
              begin
                MotivoRejeicaoComando.Add(ACBrUtil.Strings.Poem_Zeros(CodMotivo,2));
                DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo, CodMotivo));
              end;

              MotivoLinha := MotivoLinha + 2; // Incrementa a coluna dos motivos.
            end;
          end
         {Ssegmento U}
         else if Copy(Linha,14,1)= 'U' then
          begin

            if StrToIntDef(Copy(Linha,138,6),0) <> 0 then
               DataOcorrencia := StringToDateTimeDef( Copy(Linha,138,2)+'/'+
                                                      Copy(Linha,140,2)+'/'+
                                                      Copy(Linha,142,4),0, 'DD/MM/YYYY' );

            if StrToIntDef(Copy(Linha,146,6),0) <> 0 then
               DataCredito:= StringToDateTimeDef( Copy(Linha,146,2)+'/'+
                                                  Copy(Linha,148,2)+'/'+
                                                  Copy(Linha,150,4),0, 'DD/MM/YYYY' );

            ValorPago            := StrToFloatDef(Copy(Linha,78,15),0)/100;
            ValorMoraJuros       := StrToFloatDef(Copy(Linha,18,15),0)/100;
            ValorDesconto        := StrToFloatDef(Copy(Linha,33,15),0)/100;
            ValorAbatimento      := StrToFloatDef(Copy(Linha,48,15),0)/100;
            ValorIOF             := StrToFloatDef(Copy(Linha,63,15),0)/100;
            ValorRecebido        := StrToFloatDef(Copy(Linha,93,15),0)/100;
            ValorOutrasDespesas  := StrToFloatDef(Copy(Linha,108,15),0)/100;
            ValorOutrosCreditos  := StrToFloatDef(Copy(Linha,123,15),0)/100;
         end
        {Segmento W}
        else if Copy(Linha, 14, 1) = 'W' then
         begin
           //verifica o motivo de rejeição
           MotivoRejeicaoComando.Add(copy(Linha,29,2));
           DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(
                                              CodOcorrenciaToTipo(
                                              StrToIntDef(copy(Linha, 16, 2), 0)),
                                              StrToInt(Copy(Linha, 29, 2))));
         end;
      end;
   end;

end;

procedure TACBrBancoob.LerRetorno400(ARetorno: TStringList);
var
  ContLinha: Integer;
  Titulo   : TACBrTitulo;
  Linha, rCedente, rCNPJCPF : String;
begin

   if (copy(ARetorno.Strings[0],1,9) <> '02RETORNO') then
      raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                             'não é um arquivo de retorno do '+ Nome));

   rCedente := trim(Copy(ARetorno[0],32,8));


   ACBrBanco.ACBrBoleto.DataArquivo   := StringToDateTimeDef(Copy(ARetorno[0],95,2)+'/'+
                                                             Copy(ARetorno[0],97,2)+'/'+
                                                             Copy(ARetorno[0],99,2),0, 'DD/MM/YY' );

   ACBrBanco.ACBrBoleto.NumeroArquivo   := StrToIntDef(Copy(ARetorno[0],101,7), 0);

   ACBrBanco.ACBrBoleto.DataCreditoLanc := StringToDateTimeDef(Copy(ARetorno[1],111,2)+'/'+
                                                               Copy(ARetorno[1],113,2)+'/'+
                                                               Copy(ARetorno[1],115,2),0, 'DD/MM/YY' );
   rCNPJCPF := trim( Copy(ARetorno[1],4,14)) ;

   with ACBrBanco.ACBrBoleto do
   begin
      if LeCedenteRetorno then
      begin
        Cedente.CodigoCedente := rCedente;
        Cedente.Nome          := trim(Copy(ARetorno[0],47,30));
        Cedente.CNPJCPF       := rCNPJCPF;
        Cedente.Agencia       := trim(copy(ARetorno[1], 18, 4));
        Cedente.AgenciaDigito := trim(copy(ARetorno[1], 22, 1));
        Cedente.Conta         := trim(copy(ARetorno[1], 23, 8));
        Cedente.ContaDigito   := trim(copy(ARetorno[1], 31, 1));

        case StrToIntDef(Copy(ARetorno[1],2,2),0) of
           11: Cedente.TipoInscricao:= pFisica;
           else
              Cedente.TipoInscricao:= pJuridica;
        end;
      end;  
      ACBrBanco.ACBrBoleto.ListadeBoletos.Clear;
   end;

   for ContLinha := 1 to ARetorno.Count - 2 do
   begin
      Linha := ARetorno[ContLinha] ;

      if Copy(Linha,1,1)<> '1' then
         Continue;

      Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

      with Titulo do
      begin
         SeuNumero                   := copy(Linha,38,25);
         NumeroDocumento             := copy(Linha,117,10);
         OcorrenciaOriginal.Tipo     := CodOcorrenciaToTipo(StrToIntDef(
                                        copy(Linha,109,2),0));
         //05 = Liquidação Sem Registro

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
         ValorMoraJuros       := StrToFloatDef(Copy(Linha,267,13),0)/100;
         ValorOutrosCreditos  := StrToFloatDef(Copy(Linha,280,13),0)/100;
         ValorRecebido        := StrToFloatDef(Copy(Linha,254,13),0)/100;
         NossoNumero          := copy( Copy(Linha,63,11),Length( Copy(Linha,63,11) )-TamanhoMaximoNossoNum+1  ,TamanhoMaximoNossoNum);
         Carteira             := Copy(Linha,86,3);
         ValorDespesaCobranca := StrToFloatDef(Copy(Linha,182,7),0)/100;
         ValorOutrasDespesas  := StrToFloatDef(Copy(Linha,189,13),0)/100;

         if StrToIntDef(Copy(Linha,176,6),0) <> 0 then
            DataCredito:= StringToDateTimeDef( Copy(Linha,176,2)+'/'+
                                               Copy(Linha,178,2)+'/'+
                                               Copy(Linha,180,2),0, 'DD/MM/YY' );
      end;
   end;

end;

function TACBrBancoob.GerarRegistroHeader240(
  NumeroRemessa: Integer): String;
var
  ATipoInscricao: string;
begin
  FNumeroSequencialRegistroNoLote := 0;

  with ACBrBanco.ACBrBoleto.Cedente do
    begin
      case TipoInscricao of
        pFisica  : ATipoInscricao := '1';
        pJuridica: ATipoInscricao := '2';
      end;
      { GERAR REGISTRO-HEADER DO ARQUIVO }
      Result:= IntToStrZero(ACBrBanco.Numero, 3)        + // 1 a 3 - Código do banco
               '0000'                                   + // 4 a 7 - Lote de serviço
               '0'                                      + // 8 - Tipo de registro - Registro header de arquivo
               space(9)                                 + // 9 a 17 Uso exclusivo FEBRABAN/CNAB
               ATipoInscricao                           + // 18 - Tipo de inscrição do cedente
               PadLeft(OnlyNumber(CNPJCPF), 14, '0')    + // 19 a 32 -Número de inscrição do cedente
               PadRight(Convenio, 20, ' ')              + // 33 a 52 - Brancos - Alteração para passar no validador
               '0'                                      + // 53 - Zeros
               PadLeft(OnlyNumber(Agencia), 4, '0')     + // 54 a 57 - Código da agência do cedente
               PadRight(AgenciaDigito, 1, '0')          + // 58 - Digito agência do cedente
               PadLeft(OnlyNumber(Conta), 12, '0')      + // 59 a 70 - Número da conta do cedente
               PadRight(ContaDigito, 1, '0')            + // 71 - Digito conta do cedente
               PadRight(DigitoVerificadorAgenciaConta, 1, '0')+ // 72 - Dígito verificador Ag/Conta (zero)
               PadRight(Nome, 30, ' ')                  + // 73 a 102 - Nome do cedente
               PadRight('SICOOB', 30, ' ')              + // 103 a 132 - Nome do banco
               space(10)                                + // 133 A 142 - Brancos
               '1'                                      + // 143 - Código de Remessa (1) / Retorno (2)
               FormatDateTime('ddmmyyyy', Now)          + // 144 a 151 - Data do de geração do arquivo
               FormatDateTime('hhmmss', Now)            + // 152 a 157 - Hora de geração do arquivo
//               '000001'                                 + // 158 a 163 - Número sequencial do arquivo retorno
               PadLeft(OnlyNumber(inttostr(NumeroRemessa)), 6, '0')     + // 158 a 163 - Número sequencial do arquivo retorno  - marcio ereno 09/06/2018
               PadLeft(IntToStr(fpLayoutVersaoArquivo) , 3, '0')  + // 164 a 166 - Número da versão do layout do arquivo  //Alteração para passar no Validador
               '00000'                                  + // 167 a 171 - Zeros
               space(54)                                + // 172 a 225 - 54 Brancos
               space(3)                                 + // 226 a 228 - zeros
               space(12);                                 // 229 a 240 - Brancos
     { GERAR REGISTRO HEADER DO LOTE }
      Result:= Result + #13#10 +
               IntToStrZero(ACBrBanco.Numero, 3)       + //1 a 3 - Código do banco
               '0001'                                  + //4 a 7 - Lote de serviço
               '1'                                     + //8 - Tipo de registro - Registro header de arquivo
               'R'                                     + //9 - Tipo de operação: R (Remessa) ou T (Retorno)
               '01'                                    + //10 a 11 - Tipo de serviço: 01 (Cobrança)
               '  '                                    + //12 a 13 - Forma de lançamento: preencher com ZEROS no caso de cobrança
               PadLeft(IntToStr(fpLayoutVersaoLote), 3, '0')     + //14 a 16 - Número da versão do layout do lote
               ' '                                     + //17 - Uso exclusivo FEBRABAN/CNAB
               ATipoInscricao                          + //18 - Tipo de inscrição do cedente
               PadLeft(OnlyNumber(CNPJCPF), 15, '0')   + //19 a 33 -Número de inscrição do cedente
               space(20)                               + //34 a 53 - Brancos
               '0'                                     + // 54 - Zeros
               PadLeft(OnlyNumber(Agencia), 4, '0')    + //55 a 58 - Código da agência do cedente
               PadLeft(AgenciaDigito, 1, '0')          + //59 - Digito da agencia do cedente
               PadLeft(OnlyNumber(Conta), 12, '0')     + //60 - 71  Número da conta do cedente
               PadLeft(ContaDigito, 1, '0')            + //72 - Digito da conta
               ' '                                     + //73
               PadRight(Nome, 30, ' ')                 + //74 a 103 - Nome do cedente
               space(80)                               + // 104 a 183 - Brancos
               PadLeft(IntToStr(NumeroRemessa) , 08, '0') + // 184 a 191 - Número sequência do arquivo retorno.
               FormatDateTime('ddmmyyyy', Now)         + //192 a 199 - Data de geração do arquivo
               PadLeft('', 8, '0')                     + //200 a 207 - Data do crédito - Só para arquivo retorno
               space(33);                                //208 a 240 - Uso exclusivo FEBRABAN/CNAB
    end;
end;

function TACBrBancoob.GerarRegistroTransacao240(
  ACBrTitulo: TACBrTitulo): String;
var AEspecieTitulo, ATipoInscricao, ATipoOcorrencia, ATipoBoleto, ADataMoraJuros,
    ADataDesconto,ADataDesconto2,ATipoAceite,NossoNum : string;
    ATipoInscricaoAvalista: Char;
    wModalidade, ValorMora: String;
    strCarteiraEnvio : Char;
    S :string;
    MsgBoleto: Array[1..5] of string;
    K: Integer;
    ACodProtesto: Char;
   DataProtestoNegativacao: string;
   DiasProtestoNegativacao: string;
begin
  if ( ACBrTitulo.NossoNumero <> IntToStrZero(0, length(ACBrTitulo.NossoNumero)) ) then
    NossoNum  := RemoveString('-', MontarCampoNossoNumero(ACBrTitulo))
  else
    NossoNum  := ACBrTitulo.NossoNumero;
  ATipoInscricaoAvalista := ' ';
  ValorMora := '';
  with ACBrTitulo do
    begin

      S:= Mensagem.Text;
      S:= RemoveString( #13, S );
      S:= RemoveString( #10, S );
      for K := Low(MsgBoleto) to High(MsgBoleto) do
      begin
        MsgBoleto[K] := Copy(S, 1, 40);
        Delete(S, 1, 40);
      end;

      {SEGMENTO P}
      {Pegando o Tipo de Ocorrencia}
      case OcorrenciaOriginal.Tipo of
        toRemessaBaixar                        : ATipoOcorrencia := '02';
        toRemessaConcederAbatimento            : ATipoOcorrencia := '04';
        toRemessaCancelarAbatimento            : ATipoOcorrencia := '05';
        toRemessaAlterarVencimento             : ATipoOcorrencia := '06';
        toRemessaConcederDesconto              : ATipoOcorrencia := '07';
        toRemessaCancelarDesconto              : ATipoOcorrencia := '08';
        toRemessaProtestar                     : ATipoOcorrencia := '09';
        toRemessaCancelarInstrucaoProtestoBaixa: ATipoOcorrencia := '10';
        toRemessaCancelarInstrucaoProtesto     : ATipoOcorrencia := '11';
        toRemessaAlterarJurosMora              : ATipoOcorrencia := '12';
        toRemessaDispensarJuros                : ATipoOcorrencia := '13';
        toRemessaAlterarMulta                  : ATipoOcorrencia := '14';
        toRemessaDispensarMulta                : ATipoOcorrencia := '15';
        toRemessaAlterarPrazoLimiteRecebimento : ATipoOcorrencia := '19';
        toRemessaDispensarPrazoLimiteRecebimento:ATipoOcorrencia := '20';
        toRemessaAlterarDadosPagador           : ATipoOcorrencia := '23';
        toRemessaOutrasOcorrencias,
          toRemessaAlterarOutrosDados          : ATipoOcorrencia := '31';
      else
       ATipoOcorrencia := '01';
      end;

     {Pegando o Aceite do Titulo }
      case Aceite of
        atSim :  ATipoAceite := 'A';
        atNao :  ATipoAceite := 'N';
      end;

      {Pegando Tipo de Boleto} //Quem emite e quem distribui o boleto?
      case ACBrBoleto.Cedente.ResponEmissao of
        tbCliEmite        : ATipoBoleto := '2';
        tbBancoEmite      : ATipoBoleto := '1';
        tbBancoReemite    : ATipoBoleto := '3';
        tbBancoNaoReemite : ATipoBoleto := '4';
      end;

      {Pegando Tipo de Sacado}
      if Length(OnlyNumber(Sacado.CNPJCPF)) > 11 then
         Sacado.Pessoa:= pJuridica
      else
         Sacado.Pessoa:= pFisica;

      {Pegando especie do titulo}
      if EspecieDoc = 'CH' then
        AEspecieTitulo := '01'
      else if EspecieDoc = 'DM' then
        AEspecieTitulo := '02'
      else if EspecieDoc = 'DMI' then      // DMI Duplicata Mercantil indicada para protesto
        AEspecieTitulo := '03'             // no campo 107 e 108 tem que sair 03 - GR7 automação em 17.03.2017
      else if EspecieDoc = 'DS' then
        AEspecieTitulo := '04'
      else if EspecieDoc = 'DSI' then
        AEspecieTitulo := '05'
      else if EspecieDoc = 'DR' then
        AEspecieTitulo := '06'
      else if EspecieDoc = 'LC' then
        AEspecieTitulo := '07'
      else if EspecieDoc = 'NCC' then
        AEspecieTitulo := '08'
      else if EspecieDoc = 'NCE' then
        AEspecieTitulo := '09'
      else if EspecieDoc = 'NCI' then
        AEspecieTitulo := '10'
      else if EspecieDoc = 'NCR' then
        AEspecieTitulo := '11'
      else if EspecieDoc = 'NP' then
        AEspecieTitulo := '12'
      else if EspecieDoc = 'NPR' then
        AEspecieTitulo := '13'
      else if EspecieDoc = 'TM' then
        AEspecieTitulo := '14'
      else if EspecieDoc = 'TS' then
        AEspecieTitulo := '15'
      else if EspecieDoc = 'NS' then
        AEspecieTitulo := '16'
      else if EspecieDoc = 'RC' then
        AEspecieTitulo := '17'
      else if EspecieDoc = 'FAT' then
        AEspecieTitulo := '18'
      else if EspecieDoc = 'ND' then
        AEspecieTitulo := '19'
      else if EspecieDoc = 'AP' then
        AEspecieTitulo := '20'
      else if EspecieDoc = 'ME' then
        AEspecieTitulo := '21'
      else if EspecieDoc = 'PC' then
        AEspecieTitulo := '22'
      else if EspecieDoc = 'NF' then
        AEspecieTitulo := '23'
      else if EspecieDoc = 'DD' then
        AEspecieTitulo := '24'
      else if EspecieDoc = 'BDP' then
        AEspecieTitulo := '32'
//      else if EspecieDoc = 'Outros' then
//        AEspecieTitulo := '99'
      else
        AEspecieTitulo := '99';

     {Descontos}
     if (ValorDesconto > 0) then
       begin
         if(DataDesconto > 0) then
           ADataDesconto := FormatDateTime('ddmmyyyy', DataDesconto)
         else  ADataDesconto := PadLeft('', 8, '0');
       end
     else
       ADataDesconto := PadLeft('', 8, '0');

     {Código para Protesto / Negativação}
      case CodigoNegativacao of
        cnProtestarCorrido :  ACodProtesto := '1';
        cnProtestarUteis   :  ACodProtesto := '2'; 
        cnNegativar        :  ACodProtesto := '8';
      else
        case TipoDiasProtesto of
          diCorridos       : ACodProtesto := '1';
          diUteis          : ACodProtesto := '2';
        else
          ACodProtesto := '3';
        end;
      end;

      {Data e Dias de Protesto / Negativação}
      if (ACodProtesto = '8') then
      begin
        DataProtestoNegativacao := DateToStr(DataNegativacao);
        DiasProtestoNegativacao := IntToStr(DiasDeNegativacao);
      end
      else
	  begin
  	    if (ACodProtesto <> '3') then
        begin
          DataProtestoNegativacao := DateToStr(DataProtesto);
          DiasProtestoNegativacao := IntToStr(DiasDeProtesto);
        end
        else
        begin
          DataProtestoNegativacao := '';
          DiasProtestoNegativacao := '0';
        end;
	  end;

     if CodigoMora = '' then
     begin
      CodigoMora := '0'; //assume como cjIsento
       // cjValorDia, cjTaxaMensal, cjIsento
      if ValorMoraJuros > 0 then // Se tem juro atribuido, mudar de acordo com o tipo que o banco processa
      begin
        if  CodigoMoraJuros = cjValorDia then
          CodigoMora :='1'
        else if  CodigoMoraJuros = cjTaxaMensal then
          CodigoMora :='2';
      end;
     end;

     {Mora Juros}
     if (ValorMoraJuros > 0) then
       begin
         if (DataMoraJuros > 0) then
           ADataMoraJuros := FormatDateTime('ddmmyyyy', DataMoraJuros)
         else ADataMoraJuros := PadLeft('', 8, '0');
       end
     else
     begin
       ADataMoraJuros := PadLeft('', 8, '0');
       CodigoMora := '0'; // Se não tem juro atribuido, não informar o código mora
     end;

     if CodigoMora = '0' then
       ValorMora := PadLeft('', 15, '0')
     else
       ValorMora := IntToStrZero(Round(ValorMoraJuros * 100), 15);
    
     if (ACBrTitulo.CarteiraEnvio = tceCedente) then
        strCarteiraEnvio := '2'
      else
        strCarteiraEnvio := '1';

      fpValorTotalDocs:= fpValorTotalDocs  + ValorDocumento;
      Result:= IntToStrZero(ACBrBanco.Numero, 3)                             + //1 a 3 - Código do banco
               '0001'                                                        + //4 a 7 - Lote de serviço
               '3'                                                           +
               IntToStrZero((FNumeroSequencialRegistroNoLote)+ 1 ,5)                                       + //9 a 13 - Número seqüencial do registro no lote - Cada registro possui dois segmentos
               'P'                                                           + //14 - Código do segmento do registro detalhe
               ' '                                                           + //15 - Uso exclusivo FEBRABAN/CNAB: Branco
               ATipoOcorrencia                                               + //16 a 17 - Código de movimento
               '0'                                                           + // 18
               PadLeft(OnlyNumber(ACBrBoleto.Cedente.Agencia),4,'0')         + //19 a 22 - Agência mantenedora da conta
               PadLeft(ACBrBoleto.Cedente.AgenciaDigito, 1, '0')             + //23 Digito agencia
               PadLeft(OnlyNumber(ACBrBoleto.Cedente.Conta), 12, '0')        + //24 a 35 - Número da Conta Corrente
               PadLeft(ACBrBoleto.Cedente.ContaDigito , 1, '0')              + //36 - Dígito da Conta Corrente
               ' ';                                                            //37 - DV Agência/COnta Brancos
               
              wModalidade:= ifthen(trim(ACBrBoleto.Cedente.Modalidade) = '',
                                    '02', ACBrBoleto.Cedente.Modalidade);

              Result := Result+PadLeft(NossoNum, 10, '0')+ // 38 a 57 - Carteira
                        PadLeft(inttostr(ifthen(parcela=0,1,parcela)), 02, '0')+ //PadLeft('01', 02, '0')+
                        PadLeft(wModalidade, 02, '0')+
                        '4'+
                        Space(5);

               Result := Result                                           +
                         PadRight(Carteira, 1)                            + // 58 a 58 carteira
                         '0'                                              + // 59 Forma de cadastramento no banco
                         ' '                                              + // 60 Brancos
                         ATipoBoleto                                      + // 61 Identificação da emissão do boleto
                         strCarteiraEnvio                                 + // 62  Identificação da distribuição
                         PadRight(NumeroDocumento, 15, ' ')               + // 63 a 77 - Número que identifica o título na empresa [ Alterado conforme instruções da CSO Brasília ] {27-07-09}
                         FormatDateTime('ddmmyyyy', Vencimento)           + // 78 a 85 - Data de vencimento do título
                         IntToStrZero( round( ValorDocumento * 100), 15)  + // 86 a 100 - Valor nominal do título
                         '00000'                                          + // 101 a 105 - Agência cobradora. // Ficando com Zeros o Itaú definirá a agência cobradora pelo CEP do sacado
                         ' '                                              + // 106 - Dígito da agência cobradora
                         PadRight(AEspecieTitulo, 2)                      + // 107 a 108 - Espécie do documento
                         ATipoAceite                                      + // 109 - Identificação de título Aceito / Não aceito
                         FormatDateTime('ddmmyyyy', DataDocumento)        + // 110 a 117 - Data da emissão do documento
                         PadRight(CodigoMora, 1, '0')                     + // 118 - Codigo Mora (juros) - 1) Por dia, 2) Taxa mensal e 3) Isento
                         ADataMoraJuros                                   + //119 a 126 - Data a partir da qual serão cobrados juros
                         ValorMora                                        + // 127 a 141 - Valor de juros de mora por dia
                         TipoDescontoToString(TipoDesconto)               + // 142 - "Código do Desconto 1
                                                                            // '0'  =  Não Conceder desconto
                                                                            // '1'  =  Valor Fixo Até a Data Informada
                                                                            // '2'  =  Percentual Até a Data Informada"
                         ADataDesconto                                    + // 143 a 150 - Data limite para desconto
                         IfThen(ValorDesconto > 0,
                                IntToStrZero( round(ValorDesconto * 100), 15),
                                PadLeft('', 15, '0'))                     + // 151 a 165 - Valor do desconto por dia
                         IntToStrZero( round(ValorIOF * 100), 15)         + // 166 a 180 - Valor do IOF a ser recolhido
                         IntToStrZero( round(ValorAbatimento * 100), 15)  + // 181 a 195 - Valor do abatimento
                         PadRight(SeuNumero, 25, ' ')                     + // 196 a 220 - Identificação do título na empresa
                         IfThen((DataProtestoNegativacao <> '') and
                                 (StrToInt(DiasProtestoNegativacao) > 0), ACodProtesto, '3')       + // 221 - Código de protesto
                         IfThen((DataProtestoNegativacao <> '') and
                                (StrToInt(DiasProtestoNegativacao) > 0),
                                 PadLeft(DiasProtestoNegativacao, 2, '0'), '00')                   + // 222 a 223 - Prazo para protesto (em dias)
                         '0'                                              + // 224 - Código de Baixa
                         space(3)                                         + // 225 A 227 - Dias para baixa
                         '09'                                             + //
                                                              
                         PadLeft(OnlyNumber(ACBrBoleto.Cedente.Operacao), 10, '0') + // Numero contrato da operação
                         ' ';
        Inc(FNumeroSequencialRegistroNoLote);
      {SEGMENTO Q}
      {Pegando tipo de pessoa do Sacado}
      case Sacado.Pessoa of
        pFisica  : ATipoInscricao := '1';
        pJuridica: ATipoInscricao := '2';
        pOutras  : ATipoInscricao := '9';
      end;

      {Pegando tipo de pessoa do Avalista}
      if Sacado.SacadoAvalista.CNPJCPF <> '' then
       begin
        case Sacado.SacadoAvalista.Pessoa of
          pFisica  : ATipoInscricaoAvalista := '1';
          pJuridica: ATipoInscricaoAvalista := '2';
          pOutras  : ATipoInscricaoAvalista := '9';
        end;
       end
      else
       ATipoInscricaoAvalista:= '0';

      Result:= Result + #13#10 +
               IntToStrZero(ACBrBanco.Numero, 3)                          + // Código do banco
               '0001'                                                     + // Número do lote
               '3'                                                        + // Tipo do registro: Registro detalhe
               IntToStrZero((FNumeroSequencialRegistroNoLote)+ 1 ,5)       + // 9 a 13 - Número seqüencial do registro no lote - Cada registro possui dois segmentos
               'Q'                                                        + // Código do segmento do registro detalhe
               ' '                                                        + // Uso exclusivo FEBRABAN/CNAB: Branco
               ATipoOcorrencia                                            + // 16 a 17 - Código de movimento
              {Dados do sacado}
               ATipoInscricao                                             + // 18 a 18 Tipo inscricao
               PadLeft(OnlyNumber(Sacado.CNPJCPF), 15, '0')               + // 19 a 33
               PadRight(Sacado.NomeSacado, 40, ' ')                       + // 34 a 73
               PadRight(Sacado.Logradouro + ' ' + Sacado.Numero + ' ' +
                        Sacado.Complemento, 40, ' ')                      + // 74 a 113
               PadRight(Sacado.Bairro, 15, ' ')                           + // 114 a 128
               PadLeft(Sacado.CEP, 8, '0')                                + // 129 a 136
               PadRight(Sacado.Cidade, 15, ' ')                           + // 137 a 151
               PadRight(Sacado.UF, 2, ' ')                                + // 152 a 153
                        {Dados do sacador/avalista}
               ATipoInscricaoAvalista                                     + // Tipo de inscrição: Não informado
               PadLeft(OnlyNumber(Sacado.SacadoAvalista.CNPJCPF),15, '0') + // Número de inscrição
               PadRight(Sacado.SacadoAvalista.NomeAvalista, 30, ' ')      + // Nome do sacador/avalista
               space(10)                                                  + // Uso exclusivo FEBRABAN/CNAB
               PadRight('0',3, '0')                                       + // Uso exclusivo FEBRABAN/CNAB
               space(28);                                                   // Uso exclusivo FEBRABAN/CNAB
                Inc(FNumeroSequencialRegistroNoLote);
      //Registro detalhe R
      {Descontos 2}
       if (ValorDesconto2 > 0) then
         begin
           if(DataDesconto2 > 0) then
             ADataDesconto2 := FormatDateTime('ddmmyyyy', DataDesconto2)
           else  ADataDesconto2 := PadLeft('', 8, '0');
         end
       else
         ADataDesconto2 := PadLeft('', 8, '0');

      Result:= Result + #13#10 +
               IntToStrZero(ACBrBanco.Numero, 3)                          + // Código do banco
               '0001'                                                     + // Número do lote
               '3'                                                        + // Tipo do registro: Registro detalhe
               IntToStrZero((FNumeroSequencialRegistroNoLote)+ 1 ,5)       + // 9 a 13 - Número seqüencial do registro no lote - Cada registro possui dois segmentos
               'R'                                                        + // Código do segmento do registro detalhe
               ' '                                                        + // Uso exclusivo FEBRABAN/CNAB: Branco
               ATipoOcorrencia                                            + // 16 a 17 - Código de movimento
               TipoDescontoToString(TipoDesconto2)                        + // 18 - "Código do Desconto 2
                                                                            // '0'  =  Não Conceder desconto
                                                                            // '1'  =  Valor Fixo Até a Data Informada
                                                                            // '2'  =  Percentual Até a Data Informada"
               ADataDesconto2                                             + // 19 - 26 Data limite para Desconto 2
               IfThen(ValorDesconto2 > 0,
                                IntToStrZero(Round(ValorDesconto2 * 100), 15),
                                PadLeft('', 15, '0'))                     + // 27 - 41 Valor/Percentual do Desconto 2
               '0'                                                        + // 42
               PadLeft('0', 8, '0')                                       + // 43-50 data do desconto 3
               PadLeft('0', 15, '0')                                      + // 51-65 Valor ou percentual a ser concedido
               IfThen((PercentualMulta > 0),
                       IfThen(MultaValorFixo,'1','2'), '0')               + // 66 Código da multa - 1 valor fixo / 2 valor percentual / 0 Sem Multa
               IfThen((DataMulta > 0) and (PercentualMulta > 0),
                       FormatDateTime('ddmmyyyy', DataMulta),
                                      '00000000')                         + // 67 - 74 Se cobrar informe a data para iniciar a cobrança ou informe zeros se não cobrar
               IfThen((PercentualMulta > 0),
                      IntToStrZero(round(PercentualMulta * 100), 15),
                      PadLeft('', 15, '0'))                               + // 75 - 89 Percentual de multa. Informar zeros se não cobrar
               space(10);                                                   // 90-99 Informações do sacado

               if Mensagem.Count > 0 then
               begin
                 Result :=  Result + PadRight(Copy(Mensagem[0],1,40),40);    // 100-139 Menssagem livre

                 if Mensagem.Count > 1 then
                   Result := Result + PadRight(Copy(Mensagem[1],1,40),40)    // 140-179 Menssagem livre
                 else
                   Result := Result + Space(40);
               end
               else
                 Result := Result + Space(80);

               Result := Result +
               space(20)                                                  + // 180-199 Uso da FEBRABAN "Brancos"
              //   PadLeft('0', 8, '0')                                      + // 200-207 Código oco. sacado "0000000"
              IfThen((DataLimitePagto > 0),
                     FormatDateTime('ddmmyyyy', DataLimitePagto),'00000000') +
               PadLeft('0', 3, '0')                                       + // 208-210 Código do banco na conta de débito "000"
               PadLeft('0', 5, '0')                                       + // 211-215 Código da ag. debito
               ' '                                                        + // 216 Digito da agencia
               PadLeft('0', 12, '0')                                      + // 217-228 Conta corrente para debito
               ' '                                                        + // 229 Digito conta de debito
               ' '                                                        + // 230 Dv agencia e conta
               '0'                                                        + // 231 Aviso debito automatico
               space(9);                                                    // 232-240 Uso FEBRABAN
           Inc(FNumeroSequencialRegistroNoLote);
      //Registro detalhe S
      Result:= Result + #13#10 +
               IntToStrZero(ACBrBanco.Numero, 3)  + // Código do banco
               '0001'                             + // Número do lote
               '3'                                + // Tipo do registro: Registro detalhe
               IntToStrZero((FNumeroSequencialRegistroNoLote)+ 1 ,5) + // 9 a 13 - Número seqüencial do registro no lote - Cada registro possui dois segmentos
               'S'                                + // Código do segmento do registro detalhe
               ' '                                + // Uso exclusivo FEBRABAN/CNAB: Branco
               ATipoOcorrencia                    + // 16 a 17 - Código de movimento
               '3'                                + // 18 tipo impressão
               PadRight(MsgBoleto[1], 40)+ //019	a 058	040	-	Alfa	Informação 5				"Mensagem 5: Texto de observações destinado ao envio de mensagens livres, a serem impressas no campo de instruções da ficha de compensação do bloqueto.               As mensagens 5 à 9 prevalecem sobre as anteriores."
               PadRight(MsgBoleto[2], 40)+ //059	a 098	040	-	Alfa	Informação 6				"Mensagem 6: Texto de observações destinado ao envio de mensagens livres, a serem impressas no campo de instruções da ficha de compensação do bloqueto.               As mensagens 5 à 9 prevalecem sobre as anteriores."
               PadRight(MsgBoleto[3], 40)+ //099	a 138	040	-	Alfa	Informação 7				"Mensagem 7: Texto de observações destinado ao envio de mensagens livres, a serem impressas no campo de instruções da ficha de compensação do bloqueto.               As mensagens 5 à 9 prevalecem sobre as anteriores."
               PadRight(MsgBoleto[4], 40)+ //139	a 178	040	-	Alfa	Informação 8				"Mensagem 8: Texto de observações destinado ao envio de mensagens livres, a serem impressas no campo de instruções da ficha de compensação do bloqueto.               As mensagens 5 à 9 prevalecem sobre as anteriores."
               PadRight(MsgBoleto[5], 40)+ //179	a 218	040	-	Alfa	Informação 9				"Mensagem 9: Texto de observações destinado ao envio de mensagens livres, a serem impressas no campo de instruções da ficha de compensação do bloqueto.               As mensagens 5 à 9 prevalecem sobre as anteriores."
               space(22) //219 a	240	022	-	Alfa	CNAB				Uso Exclusivo FEBRABAN/CNAB: Preencher com espaços em branco
               ;                          // 217-228 Conta corrente para debito
               Inc(FNumeroSequencialRegistroNoLote);
  end;
end;

function TACBrBancoob.GerarRegistroTrailler240(
  ARemessa: TStringList): String;
begin
  {REGISTRO TRAILER DO LOTE}
  Result:= IntToStrZero(ACBrBanco.Numero, 3)                          + //Código do banco
           '0001'                                                     + //Número do lote
           '5'                                                        + //Tipo do registro: Registro trailer do lote
           Space(9)                                                   + //Uso exclusivo FEBRABAN/CNAB
           IntToStrZero(((4 * (ARemessa.Count-1))+2), 6)              + //Quantidade de Registro da Remessa
           IntToStrZero(ARemessa.Count-1, 6)                          + // Quantidade de títulos em cobrança simples
           IntToStrZero( round( fpValorTotalDocs * 100), 17)          + //Valor dos títulos em cobrança simples
           PadLeft('', 6, '0')                                        + //Quantidade títulos em cobrança vinculada
           PadLeft('',17, '0')                                        + //Valor dos títulos em cobrança vinculada
           PadLeft('',46, '0')                                        + //Complemento
           PadRight('', 8, ' ')                                       + //Referencia do aviso bancario
           space(117);
  {GERAR REGISTRO TRAILER DO ARQUIVO}
  Result:= Result + #13#10 +
           IntToStrZero(ACBrBanco.Numero, 3)                          + //Código do banco
           '9999'                                                     + //Lote de serviço
           '9'                                                        + //Tipo do registro: Registro trailer do arquivo
           space(9)                                                   + //Uso exclusivo FEBRABAN/CNAB}
           '000001'                                                   + //Quantidade de lotes do arquivo}
           IntToStrZero(((4 * (ARemessa.Count-1))+4), 6)              + //Quantidade de registros do arquivo, inclusive este registro que está sendo criado agora}
           PadLeft('', 6, '0')                                        + //Complemento
           space(205);
   fpValorTotalDocs := 0;
end;

function TACBrBancoob.CodMotivoRejeicaoToDescricao(
  const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: Integer): String;
begin
  case TipoOcorrencia of
    //Códigos de rejeições de '01' a '95' associados aos códigos de movimento '02', '03', '26' e '30'
    toRetornoRegistroConfirmado, toRetornoRegistroRecusado,
    toRetornoInstrucaoRejeitada,toRetornoAlteracaoDadosRejeitados:
      case CodMotivo  of
        00: Result := 'Outros Motivos';
        01: Result := 'Código do Banco Inválido';
        02: Result := 'Código do Registro Detalhe Inválido';
        03: Result := 'Código do Segmento Inválido';
        04: Result := 'Código de Movimento Não Permitido para Carteira';
        05: Result := 'Código de Movimento Inválido';
        06: Result := 'Tipo/Número de Inscrição do Beneficiário Inválidos';
        07: Result := 'Agência/Conta/DV Inválido';
        08: Result := 'Nosso Número Inválido';
        09: Result := 'Nosso Número Duplicado';
        10: Result := 'Carteira Inválida';
        11: Result := 'Forma de Cadastramento do Título Inválido';
        12: Result := 'Tipo de Documento Inválido';
        13: Result := 'Identificação da Emissão do Boleto de Pagamento Inválida';
        14: Result := 'Identificação da Distribuição do Boleto de Pagamento Inválida';
        15: Result := 'Características da Cobrança Incompatíveis';
        16: Result := 'Data de Vencimento Inválida';
        17: Result := 'Data de Vencimento Anterior a Data de Emissão';
        18: Result := 'Vencimento Fora do Prazo de Operação';
        19: Result := 'Título a Cargo de Bancos Correspondentes com Vencimento Inferior a XX Dias';
        20: Result := 'Valor do Título Inválido';
        21: Result := 'Espécie do Título Inválida';
        22: Result := 'Espécie do Título Não Permitida para a Carteira';
        23: Result := 'Aceite Inválido';
        24: Result := 'Data da Emissão Inválida';
        25: Result := 'Data da Emissão Posterior a Data de Entrada';
        26: Result := 'Código de Juros de Mora Inválido';
        27: Result := 'Valor/Taxa de Juros de Mora Inválido';
        28: Result := 'Código do Desconto Inválido';
        29: Result := 'Valor do Desconto Maior ou Igual ao Valor do Título';
        30: Result := 'Desconto a Conceder Não Confere';
        31: Result := 'Concessão de Desconto - Já Existe Desconto Anterior';
        32: Result := 'Valor do IOF Inválido';
        33: Result := 'Valor do Abatimento Inválido';
        34: Result := 'Valor do Abatimento Maior ou Igual ao Valor do Título';
        35: Result := 'Valor a Conceder Não Confere';
        36: Result := 'Concessão de Abatimento - Já Existe Abatimento Anterior';
        37: Result := 'ódigo para Protesto Inválido';
        38: Result := 'Prazo para Protesto Inválido';
        39: Result := 'Pedido de Protesto Não Permitido para o Título';
        40: Result := 'Título com Ordem de Protesto Emitida';
        41: Result := 'Pedido de Cancelamento/Sustação para Títulos sem Instrução de Protesto';
        42: Result := 'Código para Baixa/Devolução Inválido';
        43: Result := 'Prazo para Baixa/Devolução Inválido';
        44: Result := 'Código da Moeda Inválido';
        45: Result := 'Nome do Pagador Não Informado';
        46: Result := 'Tipo/Número de Inscrição do Pagador Inválidos';
        47: Result := 'Endereço do Pagador Não Informado';
        48: Result := 'CEP Inválido';
        49: Result := 'CEP Sem Praça de Cobrança (Não Localizado)';
        50: Result := 'CEP Referente a um Banco Correspondente';
        51: Result := 'CEP incompatível com a Unidade da Federação';
        52: Result := 'Unidade da Federação Inválida';
        53: Result := 'Tipo/Número de Inscrição do Sacador/Avalista Inválidos';
        54: Result := 'Sacador/Avalista Não Informado';
        55: Result := 'Nosso número no Banco Correspondente Não Informado';
        56: Result := 'Código do Banco Correspondente Não Informado';
        57: Result := 'Código da Multa Inválido';
        58: Result := 'Data da Multa Inválida';
        59: Result := 'Valor/Percentual da Multa Inválido';
        60: Result := 'Movimento para Título Não Cadastrado';
        61: Result := 'Alteração da Agência Cobradora/DV Inválida';
        62: Result := 'Tipo de Impressão Inválido';
        63: Result := 'Entrada para Título já Cadastrado';
        64: Result := 'Número da Linha Inválido';
        65: Result := 'Código do Banco para Débito Inválido';
        66: Result := 'Agência/Conta/DV para Débito Inválido';
        67: Result := 'Dados para Débito incompatível com a Identificação da Emissão do Boleto de Pagamento';
        68: Result := 'Débito Automático Agendado';
        69: Result := 'Débito Não Agendado - Erro nos Dados da Remessa';
        70: Result := 'Débito Não Agendado - Pagador Não Consta do Cadastro de Autorizante';
        71: Result := 'Débito Não Agendado - Beneficiário Não Autorizado pelo Pagador';
        72: Result := 'Débito Não Agendado - Beneficiário Não Participa da Modalidade Débito Automático';
        73: Result := 'Débito Não Agendado - Código de Moeda Diferente de Real (R$)';
        74: Result := 'Débito Não Agendado - Data Vencimento Inválida';
        75: Result := 'Débito Não Agendado, Conforme seu Pedido, Título Não Registrado';
        76: Result := 'Débito Não Agendado, Tipo/Num. Inscrição do Debitado, Inválido';
        77: Result := 'Transferência para Desconto Não Permitida para a Carteira do Título';
        78: Result := 'Data Inferior ou Igual ao Vencimento para Débito Automático';
        79: Result := 'Data Juros de Mora Inválido';
        80: Result := 'Data do Desconto Inválida';
        81: Result := 'Tentativas de Débito Esgotadas - Baixado';
        82: Result := 'Tentativas de Débito Esgotadas - Pendente';
        83: Result := 'Limite Excedido';
        84: Result := 'Número Autorização Inexistente';
        85: Result := 'Título com Pagamento Vinculado';
        86: Result := 'Seu Número Inválido';
        87: Result := 'e-mail/SMS enviado';
        88: Result := 'e-mail Lido';
        89: Result := 'e-mail/SMS devolvido - endereço de e-mail ou número do celular incorreto ‘90’= e-mail devolvido - caixa postal cheia';
        91: Result := 'e-mail/número do celular do Pagador não informado';
        92: Result := 'Pagador optante por Boleto de Pagamento Eletrônico - e-mail não enviado';
        93: Result := 'Código para emissão de Boleto de Pagamento não permite envio de e-mail';
        94: Result := 'Código da Carteira inválido para envio e-mail.';
        95: Result := 'Contrato não permite o envio de e-mail';
        96: Result := 'Número de contrato inválido';
        97: Result := 'Rejeição da alteração do prazo limite de recebimento (a data deve ser informada no campo 28.3.p)';
        98: Result := 'Rejeição de dispensa de prazo limite de recebimento';
        99: Result := 'Rejeição da alteração do número do título dado pelo Beneficiário';
        101 { A1 } : Result := 'Rejeição da alteração do número controle do participante';
        102 { A2 } : Result := 'Rejeição da alteração dos dados do Pagador';
        103 { A3 } : Result := 'Rejeição da alteração dos dados do Sacador/avalista';
        104 { A4 } : Result := 'Pagador DDA';
        105 { A5 } : Result := 'Registro Rejeitado – Título já Liquidado';
        106 { A6 } : Result := 'Código do Convenente Inválido ou Encerrado';
        107 { A7 } : Result := 'Título já se encontra na situação Pretendida';
        108 { A8 } : Result := 'Valor do Abatimento inválido para cancelamento';
        109 { A9 } : Result := 'Não autoriza pagamento parcial';
        201 { B1 } : Result := 'Autoriza recebimento parcial';
        202 { B2 } : Result := 'Valor Nominal do Título Conflitante';
        203 { B3 } : Result := 'Tipo de Pagamento Inválido';
        204 { B4 } : Result := 'Valor Máximo/Percentual Inválido';
        205 { B5 } : Result := 'Valor Mínimo/Percentual Inválido';
        else
          Result := IntToStrZero(CodMotivo, 2) + ' - Outros Motivos';
      end;

    //Códigos de tarifas / custas de '01' a '20' associados ao código de movimento '28'
    toRetornoDebitoTarifas:
      case CodMotivo of
        01: Result := 'Tarifa de Extrato de Posição';
        02: Result := 'Tarifa de Manutenção de Título Vencido';
        03: Result := 'Tarifa de Sustação';
        04: Result := 'Tarifa de Protesto';
        05: Result := 'Tarifa de Outras Instruções';
        06: Result := 'Tarifa de Outras Ocorrências';
        07: Result := 'Tarifa de Envio de Duplicata ao Pagador';
        08: Result := 'Custas de Protesto';
        09: Result := 'Custas de Sustação de Protesto';
        10: Result := 'Custas de Cartório Distribuidor';
        11: Result := 'Custas de Edital';
        12: Result := 'Tarifa Sobre Devolução de Título Vencido';
        13: Result := 'Tarifa Sobre Registro Cobrada na Baixa/Liquidação';
        14: Result := 'Tarifa Sobre Reapresentação Automática';
        15: Result := 'Tarifa Sobre Rateio de Crédito';
        16: Result := 'Tarifa Sobre Informações Via Fax';
        17: Result := 'Tarifa Sobre Prorrogação de Vencimento';
        18: Result := 'Tarifa Sobre Alteração de Abatimento/Desconto';
        19: Result := 'Tarifa Sobre Arquivo mensal (Em Ser)';
        20: Result := 'Tarifa Sobre Emissão de Boleto de Pagamento Pré-Emitido pelo Banco';
        else
          Result := IntToStrZero(CodMotivo, 2) + ' - Outros Motivos';
      end;

    //Códigos de liquidação / baixa de '01' a '15' associados aos códigos de movimento '06', '04'. '09' e '17'
    toRetornoLiquidado, toRetornoTransferenciaCarteira, toRetornoBaixaSimples,
    toRetornoLiquidadoAposBaixaOuNaoRegistro, toRetornoBaixaPorProtesto:  
      case CodMotivo of
        //Liquidação
        01: Result := 'Por Saldo';
        02: Result := 'Por Conta';
        03: Result := 'Liquidação no Guichê de Caixa em Dinheiro';
        04: Result := 'Compensação Eletrônica';
        05: Result := 'Compensação Convencional';
        06: Result := 'Por Meio Eletrônico';
        07: Result := 'Após Feriado Local';
        08: Result := 'Em Cartório';
        30: Result := 'Liquidação no Guichê de Caixa em Cheque';
        31: Result := 'Liquidação em banco correspondente';
        32: Result := 'Liquidação Terminal de Auto-Atendimento';
        33: Result := 'Liquidação na Internet (Home banking)';
        34: Result := 'Liquidado Office Banking';
        35: Result := 'Liquidado Correspondente em Dinheiro';
        36: Result := 'Liquidado Correspondente em Cheque';
        37: Result := 'Liquidado por meio de Central de Atendimento (Telefone)';
        // Baixa
        09: Result := 'Comandada Banco';
        10: Result := 'Comandada Cliente Arquivo';
        11: Result := 'Comandada Cliente On-line';
        12: Result := 'Decurso Prazo - Cliente';
        13: Result := 'Decurso Prazo - Banco';
        14: Result := 'Protestado';
        15: Result := 'Título Excluído';
        else
          Result := IntToStrZero(CodMotivo, 2) + ' - Outros Motivos';
      end;
  end;

  Result := ACBrSTr(Result);
end;

function TACBrBancoob.CodOcorrenciaToTipo(
  const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
      02: Result := toRetornoRegistroConfirmado;
      03: Result := toRetornoRegistroRecusado;
      04: Result := toRetornoTransferenciaCarteiraEntrada;
      05: Result := toRetornoTransferenciaCarteiraBaixa;
      06: Result := toRetornoLiquidado;
      07: Result := toRetornoRecebimentoInstrucaoConcederDesconto;
      08: Result := toRetornoRecebimentoInstrucaoCancelarDesconto;
      09: Result := toRetornoBaixaSimples;
      10: Result := toRetornoBaixaSolicitada;
      11: Result := toRetornoTituloEmSer;
      12: Result := toRetornoAbatimentoConcedido;
      13: Result := toRetornoAbatimentoCancelado;
      14: Result := toRetornoVencimentoAlterado;
      15: Result := toRetornoLiquidadoEmCartorio;
      17: Result := toRetornoLiquidadoAposBaixaOuNaoRegistro;
      19: Result := toRetornoRecebimentoInstrucaoProtestar;
      20: Result := toRetornoRecebimentoInstrucaoSustarProtesto;
      21: Result := toRetornoRecebimentoInstrucaoProtestar;
      23: Result := toRetornoEncaminhadoACartorio;
      24: Result := toRetornoInstrucaoProtestoRejeitadaSustadaOuPendente;
      25: Result := toRetornoBaixaPorProtesto;
      26: Result := toRetornoInstrucaoRejeitada;
      27: Result := toRetornoDadosAlterados;
      28: Result := toRetornoDebitoTarifas;
      30: Result := toRetornoAlteracaoDadosRejeitados;
      40: Result := toRetornoRecebimentoInstrucaoAlterarTipoCobranca;
      42: Result := toRetornoRecebimentoInstrucaoAlterarTipoCobranca;
      43: Result := toRetornoRecebimentoInstrucaoAlterarTipoCobranca;
      48: Result := toRetornoConfInstrucaoTransferenciaCarteiraModalidadeCobranca;
      51: Result := toRetornoTarifaMensalRefEntradasBancosCorrespCarteira;
      52: Result := toRetornoTarifaMensalBaixasCarteira;
      53: Result := toRetornoTarifaMensalBaixasBancosCorrespCarteira;
      98: Result := toRetornoProtestado;
      99: Result := toRetornoRegistroRecusado;

   else
      Result := toRetornoOutrasOcorrencias;
   end;

end;

function TACBrBancoob.CodOcorrenciaToTipoRemessa(const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02 : Result:= toRemessaBaixar;                          {Pedido de Baixa}
    04 : Result:= toRemessaConcederAbatimento;              {Concessão de Abatimento}
    05 : Result:= toRemessaCancelarAbatimento;              {Cancelamento de Abatimento concedido}
    06 : Result:= toRemessaAlterarVencimento;               {Alteração de vencimento}
    08 : Result:= toRemessaAlterarNumeroControle;           {Alteração de seu número}
    09 : Result:= toRemessaProtestar;                       {Pedido de protesto}
    10 : Result:= toRemessaCancelarInstrucaoProtestoBaixa;  {Instrução para sustar protesto}
    11 : Result:= toRemessaDispensarJuros;                  {Instrução para dispensar juros}
    12 : Result:= toRemessaAlterarDadosPagador;             {Alteração de Pagador}
    31 : Result:= toRemessaOutrasOcorrencias;               {Alteração de Outros Dados}
    34 : Result:= toRemessaBaixaporPagtoDiretoCedente;      {Baixa - Pagamento Direto ao Beneficiário}
  else
     Result:= toRemessaRegistrar;                           {Remessa}
  end;
end;

function TACBrBancoob.TipoOcorrenciaToCod(
  const TipoOcorrencia: TACBrTipoOcorrencia): String;
begin
  case TipoOcorrencia of
      toRetornoRegistroConfirmado                           : Result :='02';
      toRetornoRegistroRecusado                             : Result :='03';
      toRetornoTransferenciaCarteiraEntrada                 : Result :='04';
      toRetornoTransferenciaCarteiraBaixa                   : Result :='05';
      toRetornoLiquidado                                    : Result :='06';
      toRetornoBaixaTransferenciaParaDesconto               : Result :='07';
      toRetornoBaixaSimples                                 : Result :='09';
      toRetornoBaixaSolicitada                              : Result :='10';
      toRetornoTituloEmSer                                  : Result :='11';
      toRetornoAbatimentoConcedido                          : Result :='12';
      toRetornoAbatimentoCancelado                          : Result :='13';
      toRetornoVencimentoAlterado                           : Result :='14';
      toRetornoLiquidadoEmCartorio                          : Result :='15';
      toRetornoRecebimentoInstrucaoProtestar                : Result :='19';
      toRetornoDebitoEmConta                                : Result :='20';
      toRetornoNomeSacadoAlterado                           : Result :='21';
      toRetornoEnderecoSacadoAlterado                       : Result :='22';
      toRetornoEncaminhadoACartorio                         : Result :='23';
      toRetornoInstrucaoProtestoRejeitadaSustadaOuPendente  : Result :='24';
      toRetornoBaixaPorProtesto                             : Result :='25';
      toRetornoInstrucaoRejeitada                           : Result :='26';
      toRetornoDadosAlterados                               : Result :='27';
      toRetornoDebitoTarifas                                : Result :='28';
      toRetornoAlteracaoDadosRejeitados                     : Result :='30';
      toRetornoConfInstrucaoTransferenciaCarteiraModalidadeCobranca : Result :='48';
      toRetornoDespesasProtesto                             : Result :='96';
      toRetornoDespesasSustacaoProtesto                     : Result :='97';
      toRetornoDebitoCustasAntecipadas                      : Result :='98';
   else
      Result:= '02';
   end;

end;

function TACBrBancoob.TipoOcorrenciaToDescricao(
  const TipoOcorrencia: TACBrTipoOcorrencia): String;
var
   CodOcorrencia: Integer;
begin
   CodOcorrencia := StrToIntDef(TipoOcorrenciaToCod(TipoOcorrencia),0);

   case CodOcorrencia of
      02: Result:='02-CONFIRMAÇÃO ENTRADA TÍTULO' ;
      03: Result:='03-COMANDO RECUSADO' ;
      04: Result:='04-TRANSFERENCIA DE CARTEIRA - ENTRADA' ;
      05: Result:='05-LIQUIDAÇÃO SEM REGISTRO';
      06: Result:='06-LIQUIDAÇÃO NORMAL' ;
      09: Result:='09-BAIXA DE TÍTULO' ;
      10: Result:='10-BAIXA SOLICITADA';
      11: Result:='11-TÍTULOS EM SER' ;
      12: Result:='12-ABATIMENTO CONCEDIDO' ;
      13: Result:='13-ABATIMENTO CANCELADO' ;
      14: Result:='14-ALTERAÇÃO DE VENCIMENTO' ;
      15: Result:='15-LIQUIDAÇÃO EM CARTÓRIO' ;
      19: Result:='19-CONFIRMAÇÃO INSTRUÇÃO PROTESTO' ;
      20: Result:='20-DÉBITO EM CONTA' ;
      21: Result:='21-ALTERAÇÃO DE NOME DO SACADO' ;
      22: Result:='22-ALTERAÇÃO DE ENDEREÇO SACADO' ;
      23: Result:='23-ENCAMINHADO A PROTESTO' ;
      24: Result:='24-SUSTAR PROTESTO' ;
      25: Result:='25-BAIXADO E PROTESTADO' ; 
      26: Result:='26-INSTRUÇÃO REJEITADA' ;
      27: Result:='27-CONFIRMAÇÃO ALTERAÇÃO DADOS' ;
      28: Result:='28-DÉBITO DE TARIFAS/CUSTAS' ;
      30: Result:='30-ALTERAÇÃO DADOS REJEITADA' ;
      48: Result:='48-CONFIRMAÇÃO INSTR. TRANSFERENCIA DE CARTEIRA';
      96: Result:='96-DESPESAS DE PROTESTO' ;
      97: Result:='97-DESPESAS DE SUSTAÇÃO DE PROTESTO' ;
      98: Result:='98-DESPESAS DE CUSTAS ANTECIPADAS' ;
   end;

   Result := ACBrSTr(Result);
end;

end.
