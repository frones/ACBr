{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:   Juliana Rodrigues Prado                       }
{                            :   Agnaldo Pedroni                               }
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

unit ACBrBancoMercantil;

interface

uses
  Classes, SysUtils,
  ACBrBoleto, ACBrBoletoConversao;

type

  { TACBrBancoMercantil }

  TACBrBancoMercantil = class(TACBrBancoClass)
  private
    function FormataNossoNumero(const ACBrTitulo :TACBrTitulo): String;
  public
    Constructor create(AOwner: TACBrBanco);
    function CalcularDigitoVerificador(const ACBrTitulo:TACBrTitulo): String; override;
    function MontarCodigoBarras(const ACBrTitulo : TACBrTitulo): String; override;
    function MontarCampoNossoNumero(const ACBrTitulo :TACBrTitulo): String; override;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String; override;
    procedure GerarRegistroHeader400(NumeroRemessa : Integer; aRemessa: TStringList); override;
    procedure GerarRegistroTransacao400(ACBrTitulo : TACBrTitulo; aRemessa: TStringList); override;
    procedure GerarRegistroTrailler400(ARemessa:TStringList);  override;

    function CodOcorrenciaToTipoRemessa(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
  end;

implementation

uses
  {$IFDEF COMPILER6_UP} dateutils {$ELSE} ACBrD5 {$ENDIF},
  StrUtils, ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings, ACBrUtil.DateTime;

{ TACBrBancoMercantil }

function TACBrBancoMercantil.CodOcorrenciaToTipoRemessa(const CodOcorrencia: Integer): TACBrTipoOcorrencia;
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

constructor TACBrBancoMercantil.create(AOwner: TACBrBanco);
begin
   inherited create(AOwner);
   fpDigito                := 1;
   fpNome                  := 'Banco Mercantil';
   fpNumero                := 389;
   fpTamanhoMaximoNossoNum := 6;
   fpTamanhoCarteira       := 2;
end;

function TACBrBancoMercantil.FormataNossoNumero(const ACBrTitulo :TACBrTitulo): String;
var
  ANossoNumero: string;
  aModalidade: String;
begin
   aModalidade:= IfThen(Length(trim(ACBrTitulo.ACBrBoleto.Cedente.Modalidade)) = 2,
                        ACBrTitulo.ACBrBoleto.Cedente.Modalidade,'22');

   ANossoNumero := PadRight(ACBrTitulo.ACBrBoleto.Cedente.Agencia,4,'0') + //4
                   aModalidade                                       + //6
                   ACBrTitulo.Carteira                               + //8
                   PadRight(ACBrTitulo.NossoNumero, 6, '0')              + //14
                   CalcularDigitoVerificador(ACBrTitulo);              //15

   Result := ANossoNumero;
end;

function TACBrBancoMercantil.CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo ): String;
var
  aModalidade: String;
begin
   Modulo.CalculoPadrao;
   Modulo.MultiplicadorAtual   := 2;

   aModalidade:= IfThen(Length(trim(ACBrTitulo.ACBrBoleto.Cedente.Modalidade)) = 2,
                        ACBrTitulo.ACBrBoleto.Cedente.Modalidade,'22');

   Modulo.Documento := ACBrTitulo.ACBrBoleto.Cedente.Agencia +
                       aModalidade +
                       ACBrTitulo.Carteira +
                       PadRight(ACBrTitulo.NossoNumero, 6, '0');

   Modulo.Calcular;
   Result:= IntToStr(Modulo.DigitoFinal);
   
end;

function TACBrBancoMercantil.MontarCodigoBarras ( const ACBrTitulo: TACBrTitulo) : String;
var
  FatorVencimento, DigitoCodBarras, CpObrigatorio , CpLivre:String;
begin

   with ACBrTitulo.ACBrBoleto do
   begin
      FatorVencimento := CalcularFatorVencimento(ACBrTitulo.Vencimento);

      // comum a todos bancos
      CpObrigatorio := IntToStr( Numero )+  //4
                       '9'               +  //5
                       FatorVencimento   +  //9
                       IntToStrZero(Round(ACBrTitulo.ValorDocumento*100),10);//19

                       // AG+22+01+123456
      CpLivre       := FormataNossoNumero(ACBrTitulo)    + //34
                       PadRight(Cedente.CodigoCedente,9,'0') + //43
                       ifthen(ACBrTitulo.ValorDesconto > 0,'0','2') ;// ?? indicador Desconto 2-Sem 0-Com  // 44

      DigitoCodBarras := CalcularDigitoCodigoBarras(CpObrigatorio+CpLivre);

   end;

   Result:= IntToStr(Numero) +
            '9'+
            DigitoCodBarras +
            Copy( (CpObrigatorio + CpLivre),5,39);

end;

function TACBrBancoMercantil.MontarCampoNossoNumero (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
   Result := Copy(FormataNossoNumero(ACBrTitulo),5,11);
end;

// usado no carnê
function TACBrBancoMercantil.MontarCampoCodigoCedente (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
   Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia       + '-' +
             ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito + '/' +
             ACBrTitulo.ACBrBoleto.Cedente.CodigoCedente;
end;

procedure TACBrBancoMercantil.GerarRegistroHeader400(NumeroRemessa : Integer; aRemessa:TStringList);
var
  wLinha: String;
begin
   with ACBrBanco.ACBrBoleto.Cedente do
   begin
      wLinha:= '0'                                        + // ID do Registro
               '1'                                        + // ID do Arquivo( 1 - Remessa)
               'REMESSA'                                  + // Literal de Remessa
               '01'                                       + // Código do Tipo de Serviço
               PadRight( 'COBRANCA', 15 )                     + // Descrição do tipo de serviço
               PadRight( OnlyNumber(Agencia), 4)              + // agencia origem
	       PadLeft( OnlyNumber(CNPJCPF),15,'0')          + // CNPJ/CPF CEDENTE
	       ' '                                        + // BRANCO
	       PadRight( Nome, 30)                            + // Nome da Empresa
	       '389'                                      + // ID BANCO
	       'BANCO MERCANTIL'                          + // nome banco
	       FormatDateTime('ddmmyy',Now)               + // data geração
               Space(281)                                 + // espaços branco
	       '01600   '                                 + // densidade da gravação
	       IntToStrZero(NumeroRemessa,5)              + // nr. sequencial remessa
	       IntToStrZero(1,6);                           // Nr. Sequencial de Remessa

      aRemessa.Text:= aRemessa.Text + UpperCase(wLinha);
   end;
end;

procedure TACBrBancoMercantil.GerarRegistroTransacao400(ACBrTitulo :TACBrTitulo; aRemessa: TStringList);
var
  Ocorrencia,wLinha       :String;
  TipoSacado, ATipoAceite :String;
begin

   with ACBrTitulo do
   begin
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

      {Pegando Tipo de Sacado}
      case Sacado.Pessoa of
         pFisica   : TipoSacado := '01';
         pJuridica : TipoSacado := '02';
      else
         TipoSacado := '99';
      end;

      { Pegando o Aceite do Titulo }
      case Aceite of
        atSim :  ATipoAceite := 'S';
        atNao :  ATipoAceite := 'N';
      end;

      with ACBrBoleto do
      begin
         wLinha:= '1'                                                     +  // ID Registro
                  IfThen( PercentualMulta > 0, '092', '000')              +  // Indica se exite Multa ou não
	          IntToStrZero( round( PercentualMulta * 100 ), 13)       +  // Percentual de Multa formatado com 2 casas decimais
		  FormatDateTime( 'ddmmyy', Vencimento + 1)               +  // data Multa
		  Space(5)                                                +
		  PadLeft( Cedente.CodigoCedente, 9, '0')                    +  // numero do contrato ???
		  PadLeft( SeuNumero,25,'0')                                 +  // Numero de Controle do Participante
                  FormataNossoNumero(ACBrTitulo)                          +
                  Space(5)                                                +
                  PadLeft( OnlyNumber(Cedente.CNPJCPF), 15 , '0')            +
                  IntToStrZero( Round( ValorDocumento * 100 ), 10)        +  // qtde de moeda
                  '1'                                                     +  // Codigo Operação 1- Cobrança Simples
                  Ocorrencia                                              +
                  PadRight( NumeroDocumento,  10)                             + // numero titulo atribuido pelo cliente
                  FormatDateTime( 'ddmmyy', Vencimento)                   +
                  IntToStrZero(Round( ValorDocumento * 100 ),13)          +  // valor nominal do titulo
                  '389'                                                   +  // banco conbrador
                  '00000'                                                 +  // agencia cobradora
                  '01'                                                    +  // codigo da especie, duplicata mercantil
                  ATipoAceite                                             +  // N
                  FormatDateTime( 'ddmmyy', DataDocumento )               +  // Data de Emissão
                  PadRight(Instrucao1,2,'0')                                  +  // instruçoes de cobrança
                  PadRight(Instrucao2,2,'0')                                  +  // instruçoes de cobrança
                  IntToStrZero(round(ValorMoraJuros * 100),13)            + // juros mora 11.2
                  IfThen(DataDesconto > 0,
                                      FormatDateTime( 'ddmmyy', DataDesconto),
                                      PadRight('', 6, '0'))               + // data limite desconto
                  IntToStrZero(round(ValorDesconto * 100) ,13)            + // valor desconto
                  StringOfChar( '0', 13)                                  + // iof - caso seguro
                  StringOfChar( '0', 13)                                  + // valor abatimento ?????
                  TipoSacado                                              +
                  PadLeft( OnlyNumber(Sacado.CNPJCPF),14,'0')                +
                  PadRight( Sacado.NomeSacado, 40, ' ')                       +
                  PadRight( Sacado.Logradouro + Sacado.Numero , 40)           +
                  PadRight( Sacado.Bairro ,12)                                +
                  PadRight( Sacado.CEP, 8 , '0' )                             +
                  PadRight( Sacado.Cidade ,15)                                +
                  PadRight( Sacado.UF, 2)                                     +
                  PadRight( Sacado.Avalista, 30)                              + // Avalista
                  Space(12)                                               +
                  '1'                                                     + // codigo moeda
                  IntToStrZero(aRemessa.Count + 1, 6 );

         aRemessa.Text:= aRemessa.Text + UpperCase(wLinha);
      end;
   end;
end;

procedure TACBrBancoMercantil.GerarRegistroTrailler400( ARemessa:TStringList );
var
  wLinha: String;
begin
   wLinha:= '9' + Space(393)                     + // ID Registro
            IntToStrZero( ARemessa.Count + 1, 6);  // Contador de Registros

   ARemessa.Text:= ARemessa.Text + UpperCase(wLinha);
end;


end.

