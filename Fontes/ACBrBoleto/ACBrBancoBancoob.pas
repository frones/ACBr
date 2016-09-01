{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2009 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:   DOUGLAS TYBEL                                 }
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
{ Desenvolvedor desta unit: DOUGLAS TYBEL -  dtybel@yahoo.com.br  -  www.facilassim.com.br  }
{                                                                              }
{******************************************************************************}

{Somente é aceito o Convênio Carteira 1 Sem Registro} 

{$I ACBr.inc}

unit ACBrBancoBancoob;

interface

uses
  Classes, SysUtils, ACBrBoleto;

type

  { TACBrBancoob}

  TACBrBancoob = class(TACBrBancoClass)
   protected
   private
      I: Int64;
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
    function TipoOCorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia):String; override;
    function CodMotivoRejeicaoToDescricao(const TipoOcorrencia:TACBrTipoOcorrencia; CodMotivo:Integer): String; override;

   end;

implementation

uses  StrUtils, Variants, math,
      {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF},
      ACBrUtil;

constructor TACBrBancoob.create(AOwner: TACBrBanco);
begin
   inherited create(AOwner);
   fpDigito := 0;
   fpNome   := 'SICOOB';
   fpNumero := 756;
   fpTamanhoMaximoNossoNum := 7;
   fpTamanhoCarteira   := 1;
   fpTamanhoConta      := 8;
   fpCodigosMoraAceitos:= '123';
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
begin
  Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia + '/'+
            ACBrTitulo.ACBrBoleto.Cedente.CodigoCedente;
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
         toRemessaCancelarInstrucaoProtestoBaixa : Ocorrencia := '18'; {Sustar protesto e baixar}
         toRemessaCancelarInstrucaoProtesto      : Ocorrencia := '19'; {Sustar protesto e manter na carteira}
         toRemessaOutrasOcorrencias              : Ocorrencia := '31'; {Alteração de Outros Dados}
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

      if (DataProtesto > 0) then
         DiasProtesto := IntToStrZero(DaysBetween(DataProtesto,Vencimento),2)
      else
         DiasProtesto := '00';
         
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
                  PadLeft(onlyNumber(Cedente.CNPJCPF),14,'0')             +  // Número de Inscrição do Cedente
                  PadLeft(OnlyNumber(Cedente.Agencia), 4, '0')            +  // Agência
                  PadLeft( Cedente.AgenciaDigito, 1, '0')                 +  // Agência digito
                  PadLeft( OnlyNumber(Cedente.Conta)                      +  // Conta Corrente
                  Cedente.ContaDigito, 9, '0')                            +  // Dígito Conta Corrente
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
                  IntToStrZero( Round( (ValorMoraJuros * 30) *10000 ), 6) +  // Taxa de mora mês
                  IntToStrZero( Round( PercentualMulta * 10000 ), 6)      +  // Taxa de multa
                  wRespEntrega                                            +  // Responsabilidade Distribuição
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
begin
 
   if (copy(ARetorno.Strings[0],1,3) <> '756') then
      raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                             'não é um arquivo de retorno do '+ Nome));

   rCedente := trim(Copy(ARetorno[0],73,30));
   rAgencia := trim(Copy(ARetorno[0],53,5));
   rConta   := trim(Copy(ARetorno[0],63,08));
   rDigitoConta := Copy(ARetorno[0],71,1);
   ACBrBanco.ACBrBoleto.NumeroArquivo := StrToIntDef(Copy(ARetorno[0], 158, 6), 0);

   ACBrBanco.ACBrBoleto.DataArquivo   := StringToDateTimeDef(Copy(ARetorno[0],144,2)+'/'+
                                                             Copy(ARetorno[0],146,2)+'/'+
                                                             Copy(ARetorno[0],148,4),0, 'DD/MM/YY' );

   if StrToIntDef(Copy(ARetorno[1],200,6),0) <> 0 then
      ACBrBanco.ACBrBoleto.DataCreditoLanc := StringToDateTimeDef(Copy(ARetorno[0],200,2)+'/'+
                                                                  Copy(ARetorno[0],202,2)+'/'+
                                                                  Copy(ARetorno[0],204,4),0, 'DD/MM/YY' );
   rCNPJCPF := trim( Copy(ARetorno[0],19,14)) ;

   if ACBrBanco.ACBrBoleto.Cedente.TipoInscricao = pJuridica then
    begin
      rCNPJCPF := trim( Copy(ARetorno[1],19,15));
      rCNPJCPF := RightStr(rCNPJCPF,14) ;
    end
   else
    begin
      rCNPJCPF := trim( Copy(ARetorno[1],23,11));
      rCNPJCPF := RightStr(rCNPJCPF,11) ;
    end;


   with ACBrBanco.ACBrBoleto do
   begin

      if (not LeCedenteRetorno) and (rCNPJCPF <> OnlyNumber(Cedente.CNPJCPF)) then
         raise Exception.Create(ACBrStr('CNPJ\CPF do arquivo inválido'));

      if (not LeCedenteRetorno) and (StrToInt(rAgencia) <> StrToInt(Cedente.Agencia)) then
        raise Exception.CreateFMT('Agencia do arquivo %s inválida, config %s',[rAgencia,OnlyNumber(Cedente.Agencia)]);

      if (rConta + rDigitoConta <> OnlyNumber(Cedente.Conta + Cedente.ContaDigito)) then
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

         case StrToIntDef(Copy(ARetorno[1],18,1),0) of
            1: Cedente.TipoInscricao:= pFisica;
            2: Cedente.TipoInscricao:= pJuridica;
            else
               Cedente.TipoInscricao:= pJuridica;
         end;
      end;

      ACBrBanco.ACBrBoleto.ListadeBoletos.Clear;
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
            OcorrenciaOriginal.Tipo     := CodOcorrenciaToTipo(StrToIntDef(copy(Linha,16,2),0));

            //05 = Liquidação Sem Registro
            Vencimento := StringToDateTimeDef( Copy(Linha,74,2)+'/'+
                                               Copy(Linha,76,2)+'/'+
                                               Copy(Linha,80,2),0, 'DD/MM/YY' );

            ValorDocumento       := StrToFloatDef(Copy(Linha,82,15),0)/100;
            ValorDespesaCobranca := StrToFloatDef(Copy(Linha,199,15),0)/100;
            NossoNumero          := Copy(Linha,40,7);
            Carteira             := Copy(Linha,40,2);
            CodigoLiquidacao     := Copy(Linha,214,02);
            //CodigoLiquidacaoDescricao := CodigoLiquidacao_Descricao( StrToIntDef(CodigoLiquidacao,0) );

            // DONE -oJacinto Junior: Implementar a leitura dos motivos das ocorrências.
            MotivoLinha := 214;

            for I := 0 to 4 do
            begin
              CodMotivo := StrToIntDef(IfThen(Copy(Linha, MotivoLinha, 2) = '00', '00', Copy(Linha, MotivoLinha, 2)), 0);

              if CodMotivo <> 0 then
              begin
                MotivoRejeicaoComando.Add(IfThen(Copy(Linha, MotivoLinha, 2) = '00', '00', Copy(Linha, MotivoLinha, 2)));
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
        Cedente.Nome          := rCedente;
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
  I := 0;
  with ACBrBanco.ACBrBoleto.Cedente do
    begin
      case TipoInscricao of
        pFisica  : ATipoInscricao := '1';
        pJuridica: ATipoInscricao := '2';
      end;
      { GERAR REGISTRO-HEADER DO ARQUIVO }
      Result:= IntToStrZero(ACBrBanco.Numero, 3)        + //1 a 3 - Código do banco
               '0000'                                   + //4 a 7 - Lote de serviço
               '0'                                      + //8 - Tipo de registro - Registro header de arquivo
               space(9)                                 + //9 a 17 Uso exclusivo FEBRABAN/CNAB
               ATipoInscricao                           + //18 - Tipo de inscrição do cedente
               PadLeft(OnlyNumber(CNPJCPF), 14, '0')    + //19 a 32 -Número de inscrição do cedente
               StringOfChar('0', 20)                    + // 33 a 52 - Brancos - Alteração para passar no validador
               '0'                                      + // 53 - Zeros
               PadLeft(OnlyNumber(Agencia), 4, '0')        + //54 a 57 - Código da agência do cedente
               PadRight(AgenciaDigito, 1, '0')              + //58 - Digito agência do cedente
               PadLeft(OnlyNumber(Conta), 12, '0')         + // 59 a 70 - Número da conta do cedente
               PadRight(ContaDigito, 1, '0')                + //71 - Digito conta do cedente
               ' '                                      + // 72 - Dígito verificador Ag/Conta (Brancos)
               PadRight(Nome, 30, ' ')                      + // 73 a 102 - Nome do cedente
               PadRight('SICOOB', 30, ' ')                  + // 103 a 132 - Nome do banco
               space(10)                                + // 133 A 142 - Brancos
               '1'                                      + // 143 - Código de Remessa (1) / Retorno (2)
               FormatDateTime('ddmmyyyy', Now)          + // 144 a 151 - Data do de geração do arquivo
               FormatDateTime('hhmmss', Now)            + // 152 a 157 - Hora de geração do arquivo
               '000001'                                 + // 158 a 163 - Número sequencial do arquivo retorno
               '087'                                    + // 164 a 166 - Número da versão do layout do arquivo  //Alteração para passar no Validador
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
               '045'                                   + //14 a 16 - Número da versão do layout do lote
               ' '                                     + //17 - Uso exclusivo FEBRABAN/CNAB
               ATipoInscricao                          + //18 - Tipo de inscrição do cedente
               PadLeft(OnlyNumber(CNPJCPF), 15, '0')      + //19 a 33 -Número de inscrição do cedente
               space(20)                               + //34 a 53 - Brancos
               '0'                                     + // 54 - Zeros
               PadLeft(OnlyNumber(Agencia), 4, '0')       + //55 a 58 - Código da agência do cedente
               PadLeft(AgenciaDigito, 1, '0')             + //59 - Digito da agencia do cedente
               PadLeft(OnlyNumber(Conta), 12, '0')        + //60 - 71  Número da conta do cedente
               PadLeft(ContaDigito, 1, '0')               + //72 - Digito da conta
               ' '                                     + //73
               PadRight(Nome, 30, ' ')                     + //74 a 103 - Nome do cedente
               space(80)                               + // 104 a 183 - Brancos
               PadLeft(IntToStr(NumeroRemessa) , 08, '0')       + // 184 a 191 - Número sequência do arquivo retorno.
               FormatDateTime('ddmmyyyy', Now)         + //192 a 199 - Data de geração do arquivo
               PadLeft('', 8, '0')                        + //200 a 207 - Data do crédito - Só para arquivo retorno
               space(33);                                //208 a 240 - Uso exclusivo FEBRABAN/CNAB
    end;
end;

function TACBrBancoob.GerarRegistroTransacao240(
  ACBrTitulo: TACBrTitulo): String;
var AEspecieTitulo, ATipoInscricao, ATipoOcorrencia, ATipoBoleto, ADataMoraJuros,
    ADataDesconto,ATipoAceite,NossoNum : string;
    DiasProtesto: String;
    ATipoInscricaoAvalista: Char;
    wModalidade: String;
begin
  NossoNum  := RemoveString('-', MontarCampoNossoNumero(ACBrTitulo));
  ATipoInscricaoAvalista := ' ';
  with ACBrTitulo do
    begin
      {SEGMENTO P}
      {Pegando o Tipo de Ocorrencia}
      case OcorrenciaOriginal.Tipo of
        toRemessaBaixar                    : ATipoOcorrencia := '02';
        toRemessaConcederAbatimento        : ATipoOcorrencia := '04';
        toRemessaCancelarAbatimento        : ATipoOcorrencia := '05';
        toRemessaAlterarVencimento         : ATipoOcorrencia := '06';
        toRemessaConcederDesconto          : ATipoOcorrencia := '07';
        toRemessaCancelarDesconto          : ATipoOcorrencia := '08';
        toRemessaCancelarInstrucaoProtesto : ATipoOcorrencia := '10';
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
      {Pegando especie do titulo}
      if EspecieDoc = 'DM' then
        AEspecieTitulo := '02';
      {Mora Juros}
      if (ValorMoraJuros > 0) then
        begin
          if (DataMoraJuros <> Null) then
            ADataMoraJuros := FormatDateTime('ddmmyyyy', DataMoraJuros)
          else ADataMoraJuros := PadLeft('', 8, '0');
        end
      else ADataMoraJuros := PadLeft('', 8, '0');
     {Descontos}
     if (ValorDesconto > 0) then
       begin
         if(DataDesconto <> Null) then
           ADataDesconto := FormatDateTime('ddmmyyyy', DataDesconto)
         else  ADataDesconto := PadLeft('', 8, '0');
       end
     else
       ADataDesconto := PadLeft('', 8, '0');

     if (DataProtesto > 0) then
        DiasProtesto := IntToStrZero(DaysBetween(DataProtesto,Vencimento),2)
     else
        DiasProtesto := '00';


      Result:= IntToStrZero(ACBrBanco.Numero, 3)                             + //1 a 3 - Código do banco
               '0001'                                                        + //4 a 7 - Lote de serviço
               '3'                                                           +
               IntToStrZero((i)+ 1 ,5)                                       + //9 a 13 - Número seqüencial do registro no lote - Cada registro possui dois segmentos
               'P'                                                           + //14 - Código do segmento do registro detalhe
               ' '                                                           + //15 - Uso exclusivo FEBRABAN/CNAB: Branco
               ATipoOcorrencia                                               + //16 a 17 - Código de movimento
               '0'                                                           + // 18
               PadLeft(OnlyNumber(ACBrBoleto.Cedente.Agencia),4,'0')         + //19 a 22 - Agência mantenedora da conta
               PadLeft(ACBrBoleto.Cedente.AgenciaDigito, 1, '0')             + //23 Digito agencia
               PadLeft(OnlyNumber(ACBrBoleto.Cedente.Conta), 12, '0')        + //24 a 35 - Número da Conta Corrente
               PadLeft(ACBrBoleto.Cedente.ContaDigito , 1, '0')              + //36 - Dígito da Conta Corrente
               ' ';                                                            //37 - DV Agência/COnta Brancos
               if (ACBrBoleto.Cedente.ResponEmissao = tbCliEmite) then
                begin
                  wModalidade:= ifthen(trim(ACBrBoleto.Cedente.Modalidade) = '',
                                       '02', ACBrBoleto.Cedente.Modalidade);

                  Result := Result+PadLeft(NossoNum, 10, '0')+ // 38 a 57 - Carteira
                            PadLeft('01', 02, '0')+
                            PadLeft(wModalidade, 02, '0')+
                            '4'+
                            Space(5);
                end
               else
                  Result := Result+Space(20);

               Result := Result+Carteira                                  + // 58 a 58 carteira
                         '0'                                              + // 59 Forma de cadastramento no banco
                         ' '                                              + // 60 Brancos
                         ATipoBoleto                                      + // 61 Identificação da emissão do boleto
                         '2'                                              + // 62  Identificação da distribuição
                         PadRight(NumeroDocumento, 15, ' ')               + // 63 a 77 - Número que identifica o título na empresa [ Alterado conforme instruções da CSO Brasília ] {27-07-09}
                         FormatDateTime('ddmmyyyy', Vencimento)           + // 78 a 85 - Data de vencimento do título
                         IntToStrZero( round( ValorDocumento * 100), 15)  + // 86 a 100 - Valor nominal do título
                         '00000'                                          + // 101 a 105 - Agência cobradora. // Ficando com Zeros o Itaú definirá a agência cobradora pelo CEP do sacado
                         ' '                                              + // 106 - Dígito da agência cobradora
                         PadRight(AEspecieTitulo, 2)                      + // 107 a 108 - Espécie do documento
                         ATipoAceite                                      + // 109 - Identificação de título Aceito / Não aceito
                         FormatDateTime('ddmmyyyy', DataDocumento)        + // 110 a 117 - Data da emissão do documento
                         CodigoMora                                       + // 118 - Codigo Mora (juros) - 1) Por dia, 2) Taxa mensal e 3) Isento
                         ADataMoraJuros                                   + //119 a 126 - Data a partir da qual serão cobrados juros
                         IfThen(ValorMoraJuros > 0,
                                IntToStrZero( round(ValorMoraJuros * 100), 15),
                                PadLeft('', 15, '0'))                     + // 127 a 141 - Valor de juros de mora por dia
                         '0'                                              + // 142 - Zeros
                         ADataDesconto                                    + // 143 a 150 - Data limite para desconto
                         IfThen(ValorDesconto > 0,
                                IntToStrZero( round(ValorDesconto * 100), 15),
                                PadLeft('', 15, '0'))                     + // 151 a 165 - Valor do desconto por dia
                         IntToStrZero( round(ValorIOF * 100), 15)         + // 166 a 180 - Valor do IOF a ser recolhido
                         IntToStrZero( round(ValorAbatimento * 100), 15)  + // 181 a 195 - Valor do abatimento
                         PadRight(SeuNumero, 25, ' ')                     + // 196 a 220 - Identificação do título na empresa
                         '1'                                              + // 221 - Código de protesto: Protestar em XX dias corridos
                         DiasProtesto                                     + // 222 a 223 - Prazo para protesto (em dias corridos)
                         '0'                                              + // 224 - Código de Baixa
                         space(3)                                         + // 225 A 227 - Dias para baixa
                         '09'                                             + //
                         '0000000000'                                     + // Numero contrato da operação
                         ' ';
        Inc(i);
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
               IntToStrZero((i)+ 1 ,5)                                    + // 9 a 13 - Número seqüencial do registro no lote - Cada registro possui dois segmentos
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
                Inc(i);
      //Registro detalhe R
      Result:= Result + #13#10 +
               IntToStrZero(ACBrBanco.Numero, 3)                          + // Código do banco
               '0001'                                                     + // Número do lote
               '3'                                                        + // Tipo do registro: Registro detalhe
               IntToStrZero((i)+ 1 ,5)                                    + // 9 a 13 - Número seqüencial do registro no lote - Cada registro possui dois segmentos
               'R'                                                        + // Código do segmento do registro detalhe
               ' '                                                        + // Uso exclusivo FEBRABAN/CNAB: Branco
               ATipoOcorrencia                                            + // 16 a 17 - Código de movimento
               '0'                                                        + // 18  tipo de desconto 2
               PadLeft('0', 8, '0')                                       + // 19 - 26 Numero da linha a ser impressa
               PadLeft('0',15, '0')                                       + // 27 - 41 Valor/Percentual
               '0'                                                        + // 42
               PadLeft('0', 8, '0')                                       + // 43-50 data do desconto 3
               PadLeft('0', 15, '0')                                      + // 51-65 Valor ou percentual a ser concedido
               IfThen((PercentualMulta > 0), '2', '0')                    + // 66 Código da multa - 1) valor fixo e 2) valor percentual
               IfThen((PercentualMulta <> null) and (PercentualMulta > 0),
                       FormatDateTime('ddmmyyyy', DataMoraJuros),
                                      '00000000')                         + // 67 - 74 Se cobrar informe a data para iniciar a cobrança ou informe zeros se não cobrar
               IfThen(PercentualMulta > 0,
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
               PadLeft('0', 08, '0')                                      + // 200-207 Código oco. sacado "0000000"
               PadLeft('0', 3, '0')                                       + // 208-210 Código do banco na conta de débito "000"
               PadLeft('0', 5, '0')                                       + // 211-215 Código da ag. debito
               ' '                                                        + // 216 Digito da agencia
               PadLeft('0', 12, '0')                                      + // 217-228 Conta corrente para debito
               ' '                                                        + // 229 Digito conta de debito
               ' '                                                        + // 230 Dv agencia e conta
               '0'                                                        + // 231 Aviso debito automatico
               space(9);                                                    // 232-240 Uso FEBRABAN
           Inc(i);
      //Registro detalhe S
      Result:= Result + #13#10 +
               IntToStrZero(ACBrBanco.Numero, 3)  + // Código do banco
               '0001'                             + // Número do lote
               '3'                                + // Tipo do registro: Registro detalhe
               IntToStrZero((i)+ 1 ,5)            + // 9 a 13 - Número seqüencial do registro no lote - Cada registro possui dois segmentos
               'S'                                + // Código do segmento do registro detalhe
               ' '                                + // Uso exclusivo FEBRABAN/CNAB: Branco
               ATipoOcorrencia                    + // 16 a 17 - Código de movimento
               '3'                                + // 18 tipo impressão
               space(222);                          // 217-228 Conta corrente para debito
               Inc(i);
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
           IntToStrZero((3 * ARemessa.Count-1), 6)                    + //Quantidade de Registro da Remessa
           IntToStrZero(ARemessa.Count-1, 6)                          + // Quantidade de títulos em cobrança simples
           PadLeft('',17, '0')                                           + //Valor dos títulos em cobrança simples
           PadLeft('', 6, '0')                                           + //Quantidade títulos em cobrança vinculada
           PadLeft('',17, '0')                                           + //Valor dos títulos em cobrança vinculada
           PadLeft('',46, '0')                                           + //Complemento
           PadRight('', 8, ' ')                                           + //Referencia do aviso bancario
           space(117);
  {GERAR REGISTRO TRAILER DO ARQUIVO}
  Result:= Result + #13#10 +
           IntToStrZero(ACBrBanco.Numero, 3)                          + //Código do banco
           '9999'                                                     + //Lote de serviço
           '9'                                                        + //Tipo do registro: Registro trailer do arquivo
           space(9)                                                   + //Uso exclusivo FEBRABAN/CNAB}
           '000001'                                                   + //Quantidade de lotes do arquivo}
           IntToStrZero(ARemessa.Count, 6)                            + //Quantidade de registros do arquivo, inclusive este registro que está sendo criado agora}
           PadLeft('', 6, '0')                                           + //Complemento
           space(205);

end;

function TACBrBancoob.CodMotivoRejeicaoToDescricao(
  const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: Integer): String;
begin
  case TipoOcorrencia of

      //Tabela 1
      toRetornoRegistroRecusado, toRetornoEntradaRejeitadaCarne:
      case CodMotivo  of
         00: Result:='Outros Motivos';
      else
         Result := IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;


      toRetornoLiquidado:
      case CodMotivo of
         01: Result := 'POR SALDO';
         02: Result := 'POR CONTA';
         03: Result := 'NO PROPRIO BANCO';
         04: Result := 'COMPENSAÇÃO ELETRONICA';
         05: Result := 'COMPENSAÇÃO CONVENCIONAL';
         06: Result := 'POR MEIO ELETRÔNICO';
         07: Result := 'DEPOIS DE FERIADO LOCAL';
         08: Result := 'EM CARTÓRIO';

      else
         Result := IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

      toRetornoBaixado:
      case CodMotivo of
         09: Result := 'COMANDADA BANCO';
         10: Result := 'COMANDADA CLIENTE ARQUIVO';
         11: Result := 'COMANDADA CLIENTE ONLINE';
         12: Result := 'DECURSO PRAZO CLIENTE';
         13: Result := 'DECURSO PRAZO BANCO';
         14: Result := 'PROTESTADO';
         15: Result := 'TITULO EXCLUIDO';
      else
         Result := IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

  end;

end;

function TACBrBancoob.CodOcorrenciaToTipo(
  const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
      02: Result := toRetornoRegistroConfirmado;
      03: Result := toRetornoRegistroRecusado;
      05: Result := toRetornoLiquidadoSemRegistro;
      06: Result := toRetornoLiquidado;
      07: Result := toRetornoRecebimentoInstrucaoConcederDesconto;
      08: Result := toRetornoRecebimentoInstrucaoCancelarDesconto;
      09: Result := toRetornoBaixaSimples;
      11: Result := toRetornoTituloEmSer;
      12: Result := toRetornoAbatimentoConcedido;
      13: Result := toRetornoAbatimentoCancelado;
      14: Result := toRetornoVencimentoAlterado;
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
      51: Result := toRetornoTarifaMensalRefEntradasBancosCorrespCarteira;
      52: Result := toRetornoTarifaMensalBaixasCarteira;
      53: Result := toRetornoTarifaMensalBaixasBancosCorrespCarteira;
      98: Result := toRetornoProtestado;
      99: Result := toRetornoRegistroRecusado;

   else
      Result := toRetornoOutrasOcorrencias;
   end;

end;

function TACBrBancoob.TipoOCorrenciaToCod(
  const TipoOcorrencia: TACBrTipoOcorrencia): String;
begin
  case TipoOcorrencia of
      toRetornoRegistroConfirmado                           : Result :='02';
      toRetornoRegistroRecusado                             : Result :='03';
      toRetornoTransferenciaCarteira                        : Result :='04';
      toRetornoLiquidadoSemRegistro                         : Result :='05';
      toRetornoLiquidado                                    : Result :='06';
      toRetornoBaixaSimples                                 : Result :='09';
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
      toRetornoRecebimentoInstrucaoDispensarJuros           : Result :='25';
      toRetornoInstrucaoRejeitada                           : Result :='26';
      toRetornoDadosAlterados                               : Result :='27';
      toRetornoManutencaoTituloVencido                      : Result :='28';
      toRetornoAlteracaoDadosRejeitados                     : Result :='30';
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
   CodOcorrencia := StrToIntDef(TipoOCorrenciaToCod(TipoOcorrencia),0);

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
      25: Result:='25-DISPENSAR JUROS' ;
      26: Result:='26-INSTRUÇÃO REJEITADA' ;
      27: Result:='27-CONFIRMAÇÃO ALTERAÇÃO DADOS' ;
      28: Result:='28-MANUTENÇÃO TÍTULO VENCIDO' ;
      30: Result:='30-ALTERAÇÃO DADOS REJEITADA' ;
      96: Result:='96-DESPESAS DE PROTESTO' ;
      97: Result:='97-DESPESAS DE SUSTAÇÃO DE PROTESTO' ;
      98: Result:='98-DESPESAS DE CUSTAS ANTECIPADAS' ;
   end;

end;

end.
