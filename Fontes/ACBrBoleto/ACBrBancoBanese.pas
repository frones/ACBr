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

unit ACBrBancoBanese;

interface

uses
  Classes, SysUtils, Contnrs,
  ACBrBoleto, ACBrBoletoConversao;

type

  { TACBrBancoBanese }

  TACBrBancoBanese = class(TACBrBancoClass)
  private
    fValorTotalDocs:Double;
    fQtRegLote: Integer;
    fRegLote: Integer;
    fASBACE: string;
    function GetASBACE: string;
    function RetornaModalidade(const ACBrTitulo :TACBrTitulo): String;
    function FormataNossoNumero(const ACBrTitulo :TACBrTitulo): String;
  protected
    procedure EhObrigatorioContaDV; override;
    procedure EhObrigatorioAgenciaDV; override;
  public
    Constructor create(AOwner: TACBrBanco);

    property ASBACE: string read GetASBACE write fASBACE;
    function CalcularCampoASBACE(const ACBrTitulo: TACBrTitulo):string;
    function CalcularDigitoVerificador(const ACBrTitulo:TACBrTitulo): String; override;
    function MontarCodigoBarras(const ACBrTitulo : TACBrTitulo): String; override;
    function MontarCampoNossoNumero(const ACBrTitulo :TACBrTitulo): String; override;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String; override;

    function GerarRegistroHeader240(NumeroRemessa : Integer): String; override;
    function GerarRegistroTransacao240(ACBrTitulo : TACBrTitulo): String; override;
    function GerarRegistroTrailler240(ARemessa : TStringList): String;  override;
    procedure LerRetorno240(ARetorno: TStringList); override;

    procedure GerarRegistroHeader400(NumeroRemessa : Integer; ARemessa: TStringList); override;
    procedure GerarRegistroTransacao400(ACBrTitulo : TACBrTitulo; aRemessa: TStringList); override;
    procedure GerarRegistroTrailler400(ARemessa:TStringList);  override;
    Procedure LerRetorno400(ARetorno:TStringList); override;

    function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia) : String; override;
    function CodOcorrenciaToTipo(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
    function TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia):String; override;
    function CodMotivoRejeicaoToDescricao(const TipoOcorrencia:TACBrTipoOcorrencia; CodMotivo:Integer): String; override;

    function CodigoLiquidacao_Descricao( CodLiquidacao : Integer) : String;

  end;

implementation


uses StrUtils, Variants,
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF},
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.DateTime;



{ TACBrBancoBanese }

constructor TACBrBancoBanese.create(AOwner: TACBrBanco);
begin
   inherited create(AOwner);
   fpDigito                := 7;
   fpNome                  := 'Banese';
   fpNumero                := 047;
   fpTamanhoMaximoNossoNum := 9;
   fpTamanhoAgencia        := 2;
   fpTamanhoConta          := 9;
   fpTamanhoCarteira       := 2;
   fValorTotalDocs         := 0;
   fQtRegLote              := 0;
   fRegLote                := 0;
end;


procedure TACBrBancoBanese.EhObrigatorioAgenciaDV;
begin
  //sem validação
end;

procedure TACBrBancoBanese.EhObrigatorioContaDV;
begin
  //sem validação
end;

function TACBrBancoBanese.GetASBACE: string;
begin
  Result := copy(fASBACE,1,4) +' '+ copy(fASBACE,5,4) +' '+
            copy(fASBACE,9,4) +' '+ copy(fASBACE,13,4)+' '+
            copy(fASBACE,17,4)+' '+ copy(fASBACE,21,4)+' '+
            copy(fASBACE,25,1);
end;


function TACBrBancoBanese.CalcularCampoASBACE(
  const ACBrTitulo: TACBrTitulo): string;
var
  cIndice, cLivreAsbace, AContaComDigito, ANossoNumero: String;
  nContAsbace: Word;
  nResult, nResultTemp, nDigAsbace01 : Integer;
begin

  {
  AACCCCCCCCCNNNNNNNNNBBBDD
  A - agência do cedente (2 posições)
  C - conta corrente do cedente (9 posições)
  N - nosso número, incluindo o dígito (9 posições)
  B - código do banco, 047 para o Banese (3 posições)
  D - duplo dígito (2 posições)
  }

  ANossoNumero    := MontarCampoNossoNumero(ACBrTitulo);
  AContaComDigito := IntToStrZero(StrToIntDef(Trim(ACBrTitulo.ACBrBoleto.Cedente.Conta),0),8)+ACBrTitulo.ACBrBoleto.Cedente.ContaDigito;
  AContaComDigito := Copy(Trim(AContaComDigito), 1, ACBrTitulo.ACBrBoleto.Banco.TamanhoConta);

  cLivreAsbace := Trim(ACBrTitulo.ACBrBoleto.Cedente.Agencia)+
                  Trim(AContaComDigito)+
                  Trim(ANossoNumero)   +
                  '047';
  cIndice      := '21212121212121212121212';
  nResult      := 0;
  for nContAsbace := 23 downto 1 do
  begin
     nResultTemp := (StrToInt(cIndice[nContAsbace]) * StrToInt(cLivreAsbace[nContAsbace]));
     if nResultTemp > 9 then
        nResult := nResult + (nResultTemp - 9)
     else
        nResult := nResult + nResultTemp;
  end;

  nResult := nResult Mod 10;
  if nResult > 0 then
     nResult := 10 - nResult
  else nResult:=0;
     nDigAsbace01 := nResult; //guardo o primeiro dig da asbace

  cLivreAsbace := cLivreAsbace + IntToStr(nResult);
  cIndice      := '765432765432765432765432';
  nResult      := 0;

  for nContAsbace := 24 downto 1 do
    nResult := nResult + (StrToInt(cIndice[nContAsbace]) * StrToInt(cLivreAsbace[nContAsbace]));

  nResult := nResult Mod 11;
  if nResult = 0 then
     nResult := 0
  else if nResult = 1 then
   begin
     while nResult = 1 do
     begin
        nDigAsbace01 := nDigAsbace01 + 1;

        if nDigAsbace01 = 10 then
           nDigAsbace01 := 0;
        cLivreAsbace := copy(cLivreAsbace,1,23) + IntToStr(nDigAsbace01);
        cIndice      := '765432765432765432765432';
        nResult      := 0;

        for nContAsbace := 24 downto 1 do
         nResult := nResult + (StrToInt(cIndice[nContAsbace]) * StrToInt(cLivreAsbace[nContAsbace]));

        nResult := nResult Mod 11;
        if nResult = 0 then
          nResult := 0
        else if nResult > 1 then
          nResult := 11 - nResult;
     end;
   end
  else
   if nResult > 1 then
      nResult := 11 - nResult;



  cLivreAsbace := cLivreAsbace + IntToStr(nResult);
  result := cLivreAsbace;
end;


function TACBrBancoBanese.CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo ): String;
begin
   Modulo.CalculoPadrao;
   Modulo.Documento:= PadLeft(ACBrTitulo.ACBrBoleto.Cedente.Agencia, 3, '0') +
                      RightStr(ACBrTitulo.NossoNumero, 8);
   Modulo.Calcular;
   Result:= IntToStr(Modulo.DigitoFinal);
end;


function TACBrBancoBanese.MontarCodigoBarras ( const ACBrTitulo: TACBrTitulo) : String;
var
  CodigoBarras,FatorVencimento, DigitoCodBarras:String;
begin
  with ACBrTitulo.ACBrBoleto do
  begin
    fASBACE := CalcularCampoASBACE(ACBrTitulo);
    FatorVencimento := CalcularFatorVencimento(ACBrTitulo.Vencimento);

    CodigoBarras := IntToStrZero(Numero,3) + '9';
    CodigoBarras := CodigoBarras + FatorVencimento;
    CodigoBarras := CodigoBarras + IntToStrZero(Round(ACBrTitulo.ValorDocumento*100),10);
    CodigoBarras := CodigoBarras + fASBACE;
    DigitoCodBarras := CalcularDigitoCodigoBarras(CodigoBarras);
  end;

  Result:= IntToStrZero(Numero,3) + '9'+ DigitoCodBarras + Copy(CodigoBarras,5,39);
end;

function TACBrBancoBanese.MontarCampoNossoNumero (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
  result:= IntToStrZero( StrToIntDef((Trim(ACBrTitulo.NossoNumero)+Trim(CalcularDigitoVerificador(ACBrTitulo))),0) ,Self.TamanhoMaximoNossoNum);
end;

function TACBrBancoBanese.MontarCampoCodigoCedente (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
   Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia+ '/' +
             IntToStrZero(StrToIntDef(Trim(ACBrTitulo.ACBrBoleto.Cedente.Conta),0),8)+ACBrTitulo.ACBrBoleto.Cedente.ContaDigito;
end;


function TACBrBancoBanese.FormataNossoNumero(const ACBrTitulo :TACBrTitulo): String;
begin
  Result := OnlyNumber(ACBrTitulo.NossoNumero);
end;


function TACBrBancoBanese.RetornaModalidade(const ACBrTitulo :TACBrTitulo): String;
var
  AModalidade : String;
begin

  with ACBrTitulo do
  begin
    AModalidade := OnlyNumber(ACBrBoleto.Cedente.Modalidade);
  end;

  Result := AModalidade;

end;



function TACBrBancoBanese.CodigoLiquidacao_Descricao(CodLiquidacao: Integer): String;
begin

  case CodLiquidacao of
    02 : result := 'Casa Lotérica';
    03 : result := 'Agências CAIXA';
    04 : result := 'Compensação Eletrônica';
    05 : result := 'Compensação Convencional';
    06 : result := 'Internet Banking';
    07 : result := 'Correspondente Bancário';
    08 : result := 'Em Cartório'
  end;
  
end;





procedure TACBrBancoBanese.GerarRegistroHeader400(NumeroRemessa: Integer; aRemessa: TStringList );
var
  wLinha: String;
begin
   with ACBrBanco.ACBrBoleto.Cedente do begin
      wLinha:= '0'                                                + // ID do Registro
               '1'                                                + // ID do Arquivo( 1 - Remessa)
               'REMESSA'                                          + // Literal de Remessa
               '01'                                               + // Código do Tipo de Serviço
               PadRight('COBRANCA', 15 )                          +
               PadLeft(OnlyNumber(Copy(Trim(Conta),1,11)), 11,'0')+ // Codigo da Empresa no Banco
               space(9)                                           + // COMPLEMENTO DO REGISTRO
               PadRight(Nome, 30)                                 + // Nome da Empresa
               IntToStrzero(Numero,3)                             +
               PadRight('Banese', 8)                              + // Código e Nome do Banco(047 - Banese)
               space(7)                                           + // COMPLEMENTO DO REGISTRO
               FormatDateTime('ddmmyy',Now)                       +
               Space(294)                                         + // Data de geração do arquivo + brancos
               IntToStrZero(1,6);                                   // Nr. Sequencial de Remessa + brancos + Contador

      aRemessa.Text:= aRemessa.Text + UpperCase(wLinha);
   end;

end;


procedure TACBrBancoBanese.GerarRegistroTrailler400(ARemessa: TStringList);
var
  wLinha: String;
begin
   wLinha:= '9' + Space(393)                     + // ID Registro
            IntToStrZero( ARemessa.Count + 1, 6);  // Contador de Registros

   aRemessa.Text := aRemessa.Text + UpperCase(wLinha);

end;


procedure TACBrBancoBanese.GerarRegistroTransacao400(ACBrTitulo: TACBrTitulo; aRemessa: TStringList);
var
   ATipoInscricao, TipoBoleto, ATipoAceite: String;
   DigitoNossoNumero, Ocorrencia: String;
   PracaPostagem, aCarteira, Protesto: String;
   TipoSacado, MensagemCedente, wLinha: String;
   TipoAvalista: String;
begin

   case ACBrBanco.ACBrBoleto.Cedente.TipoInscricao of
      pFisica  : ATipoInscricao := '01';
      pJuridica: ATipoInscricao := '02';
   end;

   with ACBrTitulo do
   begin
      DigitoNossoNumero := CalcularDigitoVerificador(ACBrTitulo);
      aCarteira := IntToStrZero(StrToIntDef(Trim(Carteira), 0), 1);

      {Pegando Código da Ocorrencia}
      case OcorrenciaOriginal.Tipo of
         toRemessaBaixar                         : Ocorrencia := '02'; {Pedido de Baixa}
         toRemessaConcederAbatimento             : Ocorrencia := '04'; {Concessão de Abatimento}
         toRemessaCancelarAbatimento             : Ocorrencia := '05'; {Cancelamento de Abatimento concedido}
         toRemessaAlterarVencimento              : Ocorrencia := '06'; {Alteração de vencimento}
         toRemessaAlterarNumeroControle          : Ocorrencia := '08'; {Alteração de seu número}
         toRemessaProtestar                      : Ocorrencia := '09'; {Pedido de protesto}
         toRemessaCancelarInstrucaoProtestoBaixa : Ocorrencia := '20'; {Sustar protesto e baixar}
         toRemessaCancelarInstrucaoProtesto      : Ocorrencia := '18'; {Sustar protesto e manter na carteira}
         toRemessaOutrasOcorrencias              : Ocorrencia := '31'; {Alteração de Outros Dados}
      else
         Ocorrencia := '01';                                           {Remessa}
      end;

      {Pegando Tipo de Boleto}
      case ACBrBoleto.Cedente.ResponEmissao of
         tbCliEmite        : TipoBoleto  := '01';
         tbBancoNaoReemite : TipoBoleto  := '01';
         tbBancoEmite      : TipoBoleto  := '21';
         tbBancoReemite    : TipoBoleto  := '21';
      end;

      case ACBrBoleto.Cedente.ResponEmissao of
         tbCliEmite        : PracaPostagem  := '00501';
         tbBancoNaoReemite : PracaPostagem  := '00501';
         tbBancoEmite      : PracaPostagem  := '00000';
         tbBancoReemite    : PracaPostagem  := '00000';
      end;

      {Pegando campo Intruções}
      if (DataProtesto > 0) and (DataProtesto > Vencimento) then
          Protesto := 'P6' + IntToStrZero(DaysBetween(DataProtesto, Vencimento), 2)
      else
         Protesto := PadLeft(Trim(Instrucao1), 2, '0') + PadLeft(Trim(Instrucao2), 2, '0');

      {Pegando Tipo de Sacado}
      case Sacado.Pessoa of
         pFisica   : TipoSacado := '01';
         pJuridica : TipoSacado := '02';
      else
         TipoSacado := '99';
      end;

      {Pegando Tipo de Avalista}
      if Sacado.SacadoAvalista.CNPJCPF <> '' then
       begin
        case Sacado.SacadoAvalista.Pessoa of
           pFisica   : TipoAvalista := '1';
           pJuridica : TipoAvalista := '2';
        else
           TipoAvalista := '9';
        end;
       end
      else
        TipoAvalista:= '0';

      case Aceite of
         atSim :  ATipoAceite := 'A';
         atNao :  ATipoAceite := 'N';
      end;

      with ACBrBoleto do
      begin
        if Mensagem.Text<> ''then
          MensagemCedente:= Mensagem[0];
        wLinha := '1'                                                         +  // ID Registro
                  ATipoInscricao                                              +  // TIPO INSCRICAO EMPRESA(CNPJ, CPF);
                  PadRight(OnlyNumber(Cedente.CNPJCPF), 14, '0')              +
                  PadLeft(OnlyNumber(Copy(Trim(Cedente.Conta),1,11)), 11, '0')+ // Codigo da Empresa no Banco
                  Space(9)                                                    +
                  PadRight(SeuNumero,25)                                       +  // identificacao da operacao na empresa
                  PadRight(Copy(NossoNumero, 1, 8) +
                           DigitoNossoNumero, 10, '0')                        +
                  IfThen(PercentualMulta > 0, '1', '0')                       +  // Indica se exite Multa ou não
                  IntToStrZero( round( PercentualMulta * 100 ), 9)            +  // Percentual de Multa formatado com 2 casas decimais
                  Space(06)                                                   +  // identificação do carnê
                  '00'                                                        +  // número da parcela do carnê
                  '00'                                                        +  // quantidade de parcelas no carnê
                  TipoAvalista                                                +  // tipo do sacador avalista
                  PadLeft(OnlyNumber(Sacado.SacadoAvalista.CNPJCPF),14,'0')   +  // sacador avalista. não pode ser o proprio sacado
                  aCarteira                                                   +
                  Ocorrencia                                                  +
                  PadRight(NumeroDocumento, 10)                               +
                  FormatDateTime('ddmmyy', Vencimento)                        +
                  '000'                                                       +
                  IntToStrZero(Round(ValorDocumento * 100 ), 10)              +
                  IntToStrzero(Numero, 3)                                     +  // código do banco
                  PracaPostagem                                               +
                  TipoBoleto                                                  +
                  ATipoAceite                                                 +
                  FormatDateTime('ddmmyy', DataDocumento )                    +  // Data de Emissão
                  Protesto                                                    +
                  IfThen(ValorMoraJuros > 0, '0', '9')                        +  // Indica se exite Multa ou não
                  IntToStrZero(Round(ValorMoraJuros * 100 ), 12)              +
                  IfThen(DataDesconto < EncodeDate(2000, 01, 01), '000000',
                         FormatDateTime('ddmmyy', DataDesconto))              +
                  IntToStrZero(Round( ValorDesconto * 100 ), 13)              +
                  IntToStrZero(Round( ValorIOF * 100 ), 13)                   +
                  IntToStrZero(Round( ValorAbatimento * 100 ), 13)            +
                  TipoSacado                                                  +
                  PadLeft(OnlyNumber(Sacado.CNPJCPF),14,'0')                  +
                  PadRight(Sacado.NomeSacado, 40, ' ')                        +
                  PadRight(Sacado.Logradouro + Sacado.Numero, 40)             +
                  PadRight(Sacado.Bairro, 12)                                 +
                  PadRight(Sacado.CEP, 8)                                     +
                  PadRight(Sacado.Cidade,15)                                  +
                  PadRight(Sacado.UF, 2)                                      +
                  PadRight(MensagemCedente, 40)                               +
                  '00'                                                        +
                  '0';

        wLinha := wLinha + IntToStrZero(aRemessa.Count + 1 {ListadeBoletos.IndexOf(ACBrTitulo) +
                               ListadeBoletos.IndexOf(ACBrTitulo) + 2}, 6);
        aRemessa.Text := aRemessa.Text + UpperCase(wLinha);
      end;
   end;

end;


function TACBrBancoBanese.GerarRegistroHeader240(NumeroRemessa : Integer): String;
var
  ATipoInscricao: string;
  AMensagemReservada: String;
begin

   with ACBrBanco.ACBrBoleto.Cedente do
   begin
   
      case TipoInscricao of
         pFisica  : ATipoInscricao := '1';
         pJuridica: ATipoInscricao := '2';
      end;

      if ACBrBanco.ACBrBoleto.Homologacao then
         AMensagemReservada := 'REMESSA-TESTE'
      else
         AMensagemReservada := 'REMESSA-PRODUCAO';

      { GERAR REGISTRO-HEADER DO ARQUIVO }
      Result:= IntToStrZero(ACBrBanco.Numero, 3)       + //1 a 3 - Código do banco
               '0000'                                  + //4 a 7 - Lote de serviço - Se registro for Header do Arquivo preencher com '0000'
               '0'                                     + //8 - Tipo de registro - Registro header de arquivo '0' = Header de Arquivo
               PadRight('', 9, ' ')                    + //9 a 17 Uso exclusivo FEBRABAN/CNAB
               ATipoInscricao                          + //18 - Tipo de inscrição do cedente [ '1' = CPF '2' = CGC / CNPJ ]
               PadLeft(OnlyNumber(CNPJCPF), 14, '0')   + //19 a 32 -Número de inscrição do cedente CPF ou CNPJ
               PadLeft(Convenio,20, '0')               + //33 a 52 - Código do convênio no banco
               PadLeft(OnlyNumber(Agencia), 5, '0')    + //53 a 57 - Código da agência do cedente
               PadLeft(AgenciaDigito, 1 , '0')         + //58 - Dígito da agência do cedente
               PadLeft(Conta, 12, '0')                 + //59 a 70 - Código Cedente (Código do Convênio no Banco)
               PadLeft(ContaDigito, 1, '0')            + //71 - Digito verificado da conta - DEIXA BRANCO
               Space(1)                                + //72 - Digito verificado da Agencia/conta - DEIXA BRANCO
               PadRight(Nome, 30, ' ')                 + //73 a 102 - Nome do cedente
               PadRight('BANESE', 30, ' ')             + //103 a 132 - Nome do banco
               PadRight('', 10, ' ')                   + //133 a 142 - Uso exclusivo FEBRABAN/CNAB
               '1'                                     + //143 - Código de Remessa / Retorno ( '1' = Remessa (Cliente > Banco)   '2' = Retorno (Banco > Cliente) )
               FormatDateTime('ddmmyyyy', Now)         + //144 a 151 - Data do de geração do arquivo
               FormatDateTime('hhmmss', Now)           + //152 a 157 - Hora de geração do arquivo
               PadLeft(IntToStr(NumeroRemessa), 6, '0')+ //158 a 163 - Número seqüencial do arquivo
               '101'                                   + //164 a 166 - Número da versão do layout do arquivo "101"
               PadRight('',  5, '0')                   + //167 a 171 - Densidade de gravação do arquivo (BPI)
               Space(20)                               + // 172 a 191 - Uso reservado do banco
               PadLeft(AMensagemReservada, 20, ' ')    + // 192 a 211 - Uso reservado da empresa
               PadLeft('', 29, ' ');                     // 212 a 240 - Uso Exclusivo FEBRABAN / CNAB

      { GERAR REGISTRO HEADER DO LOTE }
      Result:= Result + #13#10 +
               IntToStrZero(ACBrBanco.Numero, 3)        + //1 a 3 - Código do banco
               '0001'                                   + //4 a 7 - Lote de serviço - Preencher com '0001' para o primeiro lote do arquivo.
               '1'                                      + //8 - Tipo de registro - Registro header de arquivo  '1' = Header de Lote
               'R'                                      + //9 - Tipo de operação: 'R' = Arquivo Remessa 'T' = Arquivo Retorno
               '01'                                     + //10 a 11 - Tipo de serviço: 01' = Cobrança
               Space(2)                                 + //12 a 13 - FUso Exclusivo FEBRABAN / CNAB: Preencher com Brancos.
               '060'                                    + //14 a 16 - Número da versão do layout do lote
               Space(1)                                 + //17 - Uso exclusivo FEBRABAN/CNAB
               ATipoInscricao                           + //18 - Tipo de inscrição do cedente [ '1' = CPF '2' = CGC / CNPJ ]
               PadLeft(OnlyNumber(CNPJCPF), 15, '0')    + //19 a 33 -Número de inscrição do cedente CPF OU CNPJ
               PadLeft(Convenio, 20, '0')               + //34 a 53 - Código do convênio no banco (código do Convenio)
               PadLeft(OnlyNumber(Agencia), 5 , '0')    + //54 a 58 - Agência do cedente
               PadLeft(AgenciaDigito, 1 , '0')          + //59 - Dígito da agência do cedente
               PadLeft(Conta, 12, '0')                  + //60 a 71 - Código do convênio no banco (código do cedente)
               PadLeft(ContaDigito, 1, '0')             + //71 - Digito verificado da conta - DEIXA BRANCO
               Space(1)                                 + //73 - Digito verificado da Agencia/conta - DEIXA BRANCO
               PadLeft(Nome, 30, ' ')                   + //74 a 103 - Nome do cedente
               PadLeft('', 40, ' ')                     + //104 a 143 - Mensagem 1 para todos os boletos do lote
               PadLeft('', 40, ' ')                     + //144 a 183 - Mensagem 2 para todos os boletos do lote
               PadLeft(IntToStr(NumeroRemessa), 8, '0') + //184 a 191 - Número do arquivo
               FormatDateTime('ddmmyyyy', Now)          + //192 a 199 - Data de geração do arquivo
               PadLeft('', 8, '0')                      + //200 a 207 - Data do crédito - Só para arquivo retorno
               PadLeft('', 33, ' ');                      //208 a 240 - Uso exclusivo FEBRABAN/CNAB
   end;

   
end;



function TACBrBancoBanese.GerarRegistroTransacao240(ACBrTitulo : TACBrTitulo): String;
var
  ATipoOcorrencia, ATipoBoleto, ADataMoraJuros, AModalidade        : String;
  ADataDesconto, ADataMulta, ANossoNumero, ATipoAceite, AEspecieDoc: String; 

  function MontarInstrucoes2: string;
  begin
    Result := '';
    with ACBrTitulo do
    begin
      if (Mensagem.Count <= 2) then
      begin
        if (Mensagem.Count = 2) then
          Result := Copy(PadRight(Mensagem[0] +' / '+ Mensagem[1], 140, ' '), 1, 140)
        else
          Result := Copy(PadRight(Mensagem[0], 140, ' '), 1, 140);

        Exit;
      end;

      if (Mensagem.Count >= 3) then
      begin
        Result := Copy(PadRight(Mensagem[2], 40, ' '), 1, 40);
      end;

      if (Mensagem.Count >= 4) then
      begin
        Result := Result +
                  Copy(PadRight(Mensagem[3], 40, ' '), 1, 40)
      end;

      if (Mensagem.Count >= 5) then
      begin
        Result := Result +
                  Copy(PadRight(Mensagem[4], 40, ' '), 1, 40)
      end;

      if (Mensagem.Count >= 6) then
      begin
        Result := Result +
                  Copy(PadRight(Mensagem[5], 40, ' '), 1, 40)
      end;

      if (Mensagem.Count >= 7) then
      begin
        Result := Result +
                  Copy(PadRight(Mensagem[6], 40, ' '), 1, 40)
      end;

      // Acertar a quantidade de caracteres
      Result := PadRight(Result, 200);

    end;
  end;

begin

  with ACBrTitulo do
  begin
      {Pegando a Modalidade}
      AModalidade:= RetornaModalidade(ACBrTitulo);

      if ( Trim(ACBrTitulo.NossoNumero) <> '' ) then
        ANossoNumero := FormataNossoNumero(ACBrTitulo)
      else
        ANossoNumero := '';

      {SEGMENTO P}

      //Pegando o Tipo de Ocorrencia
      ATipoOcorrencia := TipoOcorrenciaToCodRemessa(OcorrenciaOriginal.Tipo);

      //Pegando o Aceite do Titulo
      case Aceite of
         atSim :  ATipoAceite := 'A';
         atNao :  ATipoAceite := 'N';
      end;

      if AnsiSameText(EspecieDoc, 'CH') then
        AEspecieDoc := '01'
      else if AnsiSameText(EspecieDoc, 'DM') then
        AEspecieDoc := '02'
      else if AnsiSameText(EspecieDoc, 'DMI') then
        AEspecieDoc := '03'
      else if AnsiSameText(EspecieDoc, 'DS') then
        AEspecieDoc := '04'
      else if AnsiSameText(EspecieDoc, 'DSI') then
        AEspecieDoc := '05'
      else if AnsiSameText(EspecieDoc, 'DR') then
        AEspecieDoc := '06'
      else if AnsiSameText(EspecieDoc, 'LC') then
        AEspecieDoc := '07'
      else if AnsiSameText(EspecieDoc, 'NCC') then
        AEspecieDoc := '08'
      else if AnsiSameText(EspecieDoc, 'NCE') then
        AEspecieDoc := '09'
      else if AnsiSameText(EspecieDoc, 'NCI') then
        AEspecieDoc := '10'
      else if AnsiSameText(EspecieDoc, 'NCR') then
        AEspecieDoc := '11'
      else if AnsiSameText(EspecieDoc, 'NP') then
        AEspecieDoc := '12'
      else if AnsiSameText(EspecieDoc, 'NPR') then
        AEspecieDoc := '13'
      else if AnsiSameText(EspecieDoc, 'TM') then
        AEspecieDoc := '14'
      else if AnsiSameText(EspecieDoc, 'TS') then
        AEspecieDoc := '15'
      else if AnsiSameText(EspecieDoc, 'NS') then
        AEspecieDoc := '16'
      else if AnsiSameText(EspecieDoc, 'RC') then
        AEspecieDoc := '17'
      else if AnsiSameText(EspecieDoc, 'FAT') then
        AEspecieDoc := '18'
      else if AnsiSameText(EspecieDoc, 'ND') then
        AEspecieDoc := '19'
      else if AnsiSameText(EspecieDoc, 'AP') then
        AEspecieDoc := '20'
      else if AnsiSameText(EspecieDoc, 'ME') then
        AEspecieDoc := '21'
      else if AnsiSameText(EspecieDoc, 'PC') then
        AEspecieDoc := '22'
      else if AnsiSameText(EspecieDoc, 'NF') then
        AEspecieDoc := '23'
      else if AnsiSameText(EspecieDoc, 'DD') then
        AEspecieDoc := '24'
      else if AnsiSameText(EspecieDoc, 'CPR') then
        AEspecieDoc := '25'
      else
        AEspecieDoc := '99';

      //Pegando Tipo de Boleto} //Quem emite e quem distribui o boleto?
      case ACBrBoleto.Cedente.ResponEmissao of
         tbBancoEmite      : ATipoBoleto := '1' + '1';
         tbCliEmite        : ATipoBoleto := '2' + '2';
         tbBancoReemite    : ATipoBoleto := '4' + '1';
         tbBancoNaoReemite : ATipoBoleto := '5' + '2';
      end;

      {Mora Juros}
      if (ValorMoraJuros > 0) then
      begin
        if DataMoraJuros <> 0 then
            ADataMoraJuros := FormatDateTime('ddmmyyyy', DataMoraJuros)
        else
            ADataMoraJuros := PadRight('', 8, '0');
      end
      else
         ADataMoraJuros := PadRight('', 8, '0');

      {Descontos}
      if (ValorDesconto > 0) then
       begin
         if (DataDesconto <> Null) then
            ADataDesconto := FormatDateTime('ddmmyyyy', DataDesconto)
         else
            ADataDesconto := PadRight('', 8, '0');
       end
      else
         ADataDesconto := PadRight('', 8, '0');

      {Multa}
      if (PercentualMulta > 0) then
        ADataMulta := IfThen(DataMulta > 0,
                             FormatDateTime('ddmmyyyy', DataMulta),
                             FormatDateTime('ddmmyyyy', Vencimento + 1))
      else
        ADataMulta := PadLeft('', 8, '0');


      fValorTotalDocs := fValorTotalDocs  + ValorDocumento;
      Inc(fRegLote);
      Result:= IntToStrZero(ACBrBanco.Numero, 3)                          + //1 a 3 - Código do banco
               '0001'                                                     + //4 a 7 - Lote de serviço - Preencher com '0001' para o primeiro lote do arquivo.
               '3'                                                        + //8 - Tipo do registro: Registro detalhe '3' = Detalhe
               IntToStrZero(fRegLote,5)                                   + //9 a 13 - Número seqüencial do registro no lote - Cada título tem 2 registros (P e Q)
               'P'                                                        + //14 - Código do segmento do registro detalhe
               Space(1)                                                   + //15 - Uso exclusivo FEBRABAN/CNAB: Branco
               ATipoOcorrencia                                            + //16 a 17 - Código de movimento - '01' = Entrada de Títulos
               PadLeft(OnlyNumber(ACBrBoleto.Cedente.Agencia), 5, '0')    + //18 a 22 - Agência mantenedora da conta
               PadLeft(ACBrBoleto.Cedente.AgenciaDigito, 1 , '0')         + //23 -Dígito verificador da agência
               PadLeft(ACBrBoleto.Cedente.Conta, 12, '0')                 + //24 a 35 - Número da Conta Corrente
               PadLeft(ACBrBoleto.Cedente.ContaDigito, 1, '0')            + //36 a 36 - Dígito Verificador da Conta
               Space(1)                                                   + //37 a 37 - Dígito Verificador da Ag/Conta  - DEIXAR BRANCO
               PadLeft(Copy(ANossoNumero,3,17), 20, '0')                  + //38 a 57 - Nosso número - identificação do título no banco
               '1'                                                        + //58 - Código da Carteira: ['1' = Cobrança Simples]
               '1'                                                        + //59 - Forma de cadastramento do título no banco: ['1' = Com Cadastramento (Cobrança Registrada)]
               '1'                                                        + //60 - Tipo de documento:['1' = Tradicional '2' = Escritural]
               '2'                                                        + //61 - Quem emite: ['2' = Cliente Emite]
               '2'                                                        + //62 - Quem distribui: ['2' = Cliente Distribui]
               PadRight(NumeroDocumento, 15, ' ')                         + //63 a 77 - Número que identifica o título na empresa
               FormatDateTime('ddmmyyyy', Vencimento)                     + //78 a 85 - Data de vencimento do título
               IntToStrZero( round( ValorDocumento * 100), 15)            + //86 a 100 - Valor nominal do título
               PadLeft('', 5, '0')                                        + //101 a 105 - Agência cobradora. Se ficar em branco, a caixa determina automaticamente pelo CEP do sacado
               ' '                                                        + //106 - Dígito da agência cobradora
               PadLeft(AEspecieDoc, 2)                                    + //107 a 108 - Espécie do documento
               ATipoAceite                                                + //109 - Identificação de título Aceito / Não aceito: ['A' = Aceite]
               FormatDateTime('ddmmyyyy', DataDocumento)                  + //110 a 117 - Data da emissão do documento
               IfThen( (ValorMoraJuros > 0) and (CodigoMora= ''), '1', PadRight(CodigoMora, 1, '3') )                                     + //118 - Código de juros de mora: Valor por dia: ['1' = Valor por Dia:'2' = Taxa Mensal:'3' = Isento]
               ADataMoraJuros                                             + //119 a 126 - Data a partir da qual serão cobrados juros
               IfThen(ValorMoraJuros > 0, IntToStrZero( round(ValorMoraJuros * 100), 15),PadRight('', 15, '0'))                           + //127 a 141 - Valor de juros de mora por dia
               IfThen(ValorDesconto > 0, '1', '0')                        + //142 - Código de desconto: Valor fixo até a data informada:['0' = Isento/'1' = Valor Fixo Até a Data Informada/'2' = Percentual Até a Data Informada]
               ADataDesconto                                              + //143 a 150 - Data do desconto
               IfThen(ValorDesconto > 0, IntToStrZero( round(ValorDesconto * 100), 15),PadRight('', 15, '0'))                             + //151 a 165 - Valor do desconto por dia
               IntToStrZero( round(ValorIOF * 100), 15)                                 + //166 a 180 - Valor do IOF a ser recolhido
               IntToStrZero( round(ValorAbatimento * 100), 15)                          + //181 a 195 - Valor do abatimento
               PadLeft(IfThen(SeuNumero<>'',SeuNumero,NumeroDocumento), 25, '0')        + //196 a 220 - Identificação do título na empresa
               IfThen((DataProtesto <> 0) and (DiasDeProtesto > 0), '1', '3')           + //221 - Código de protesto: Protestar em XX dias corridos:['3' = Não Protestar]
               IfThen((DataProtesto <> 0) and (DiasDeProtesto > 0),
                    PadLeft(IntToStr(DiasDeProtesto), 2, '0'), '00')                    + //222 a 223 - Prazo para protesto (em dias corridos)
               IfThen(((DataProtesto = 0) or (DataProtesto <= Vencimento))
                      or ((DataBaixa <> 0) and (DataBaixa > Vencimento)), '1', '2')     + //224 - Código para baixa/devolução: ['1' = Baixar / Devolver]
               IfThen((DataBaixa <> 0) and (DataBaixa > Vencimento),
                 PadLeft(IntToStr(DaysBetween(DataBaixa, Vencimento)), 3, '0'), '060')  + //225 a 227 - Prazo para baixa/devolução (em dias corridos)
               '09'                                                                     + //228 a 229 - Código da moeda: Real
               PadLeft('', 10 , '0')                                                    + //230 a 239 - Uso Exclusivo CAIXA
               '1';                                                                       //240 - Uso exclusivo FEBRABAN/CNAB
               Inc(fQtRegLote);

      {SEGMENTO Q}
      Result:= Result + #13#10 +
               IntToStrZero(ACBrBanco.Numero, 3)                                       + //1 a 3 - Código do banco
               '0001'                                                                  + //4 a 7 - Número do lote - Preencher com '0001' para o primeiro lote do arquivo.
               '3'                                                                     + //8 - Tipo do registro: Registro detalhe '3' = Detalhe
               IntToStrZero(fRegLote,5)                                                + //9 a 13 - Número seqüencial do registro no lote - Cada título tem 2 registros (P e Q)
               'Q'                                                                     + //14 - Código do segmento do registro detalhe
               Space(1)                                                                + //15 - Uso exclusivo FEBRABAN/CNAB: Branco
               ATipoOcorrencia                                                         + //16 a 17 - Código de movimento
                   {Dados do sacado}
               IfThen(Sacado.Pessoa = pJuridica,'2','1')                               + //18 - Tipo inscricao
               PadLeft(OnlyNumber(Sacado.CNPJCPF), 15, '0')                            + //19 a 33 - Número de Inscrição
               PadLeft(Sacado.NomeSacado, 40, ' ')                                     + //34 a 73 - Nome sacado
               PadLeft(Sacado.Logradouro + ' ' + Sacado.Numero + ' ' +Sacado.Complemento , 40, ' ')  + //74 a 113 - Endereço
               PadLeft(Sacado.Bairro, 15, ' ')                                         + // 114 a 128 - bairro sacado
               PadLeft(OnlyNumber(Sacado.CEP), 8, '0')                                 + // 129 a 133 e 134 a 136- cep sacado prefixo e sufixo sem o traço"-" somente numeros
               PadLeft(Sacado.Cidade, 15, ' ')                                         + // 137 a 151 - cidade sacado
               PadLeft(Sacado.UF, 2, ' ')                                              + // 152 a 153 - UF sacado
               {Dados do sacador/avalista}
               IfThen(EstaVazio(Sacado.SacadoAvalista.NomeAValista),
                      '0',
                      IfThen(Sacado.SacadoAvalista.Pessoa = pJuridica,
                             '2',
                             '1'
                      )
               )                                                                       + // 154 a 157 - Tipo de Inscrição
               PadLeft(OnlyNumber(Sacado.SacadoAvalista.CNPJCPF), 15, '0')             + // 155 a 169 - Número de inscrição
               PadLeft(Sacado.SacadoAvalista.NomeAValista, 40, ' ')                    + // 170 a 209 - Nome do sacador/avalista
               IntToStrZero(0,3)                                                       + // 210 a 212 - Cód. Bco. Corresp. na Compensação
               IntToStrZero(0,20)                                                      + // 213 a 232 - Nosso Nº no Banco Correspondente
               Space(8);                                                                 // 233 a 240 - Uso exclusivo FEBRABAN/CNAB
               Inc(fQtRegLote);

    {Registro Detalhe - Segmento R (Opcional - Remessa)}
    Result:= Result + #13#10 +
             IntToStrZero(ACBrBanco.Numero, 3)                                           + //   1 a 3   - Código do banco
             '0001'                                                                      + //   4 a 7   - Número do lote - Preencher com '0001' para o primeiro lote do arquivo.
             '3'                                                                         + //   8 a 8   - Tipo do registro: Registro detalhe  '3' = Detalhe
             IntToStrZero(fRegLote,5)                                                    + //   9 a 13  - Número seqüencial do registro no lote - Cada título tem 2 registros (P e Q)
             'R'                                                                         + //  14 a 14  - Código do segmento do registro detalhe
             Space(1)                                                                    + //  15 a 15  - Uso exclusivo FEBRABAN/CNAB: Branco
             ATipoOcorrencia                                                             + //  16 a 17  - Tipo Ocorrencia
             PadLeft('', 1,  '0')                                                           + //  18 a 18  - Codigo do Desconto 2
             PadLeft('', 8,  '0')                                                           + //  19 a 26  - Data do Desconto 2
             PadLeft('', 15, '0')                                                           + //  27 a 41  - Valor/Percentual a ser concedido
             PadLeft('', 1,  '0')                                                           + //  42 a 42  - Código do Desconto 3
             PadLeft('', 8,  '0')                                                           + //  43 a 50  - Data do Desconto 3
             PadLeft('', 15, '0')                                                           + //  51 a 65  - Valor/Percentual a ser concedido
             IfThen((PercentualMulta > 0),
                     IfThen(MultaValorFixo,'1','2'), '0')                                   + //  66 a 66  - Código da Multa
             ADataMulta                                                                     + //  67 a 74  - Data da Multa
             IfThen(PercentualMulta > 0, IntToStrZero(round(PercentualMulta * 100), 15),
               PadRight('', 15, '0'))                                                       + //  75 a 89  - Valor/Percentual a ser aplicado
             PadRight('', 10, ' ')                                                          + //  90 a 99  - Informação ao Sacado
             PadRight('', 40, ' ')                                                          + // 100 a 139 - Mensagem 3
             PadRight('', 40, ' ')                                                          + // 140 a 179 - Mensagem 4
             PadRight('', 20, ' ')                                                          + // 180 a 199 - Uso Exclusivo FEBRABAN/CNAB
             PadRight('',  8, '0')                                                          + // 200 a 207 - Código Ocor. do Pagador
             PadRight('',  3, '0')                                                          + // 208 a 210 - Código do Banco na Conta do Débito
             PadRight('',  5, '0')                                                          + // 211 a 215 - Código da Agência do Débito
             PadRight('',  1, '0')                                                          + // 216 a 216 - Dígito Verificador da Agência
             PadRight('', 12, '0')                                                          + // 217 a 228 - Conta Corrente para Débito
             PadRight('',  1, ' ')                                                          + // 229 a 229 - Dígito Verificador da Conta
             PadRight('',  1, ' ')                                                          + // 230 a 230 - Dígito Verificador Ag/Conta
             PadRight('',  1, '0')                                                          + // 231 a 231 - Aviso para Débito Automático
             PadRight('',  9, ' ')                                                          ; // 232 a 240 - Uso Exclusivo FEBRABAN/CNAB

             Inc(fQtRegLote);

    (*
    {Registro Detalhe - Segmento Y-53 (Remessa - Opcional)}
    if (Mensagem.Count > 0) then
    begin
      Result := Result + #13#10 +
                IntToStrZero(ACBrBanco.Numero, 3)                                           + //   1 a 3   - Código do banco
                '0001'                                                                      + // 004 - 007 / Numero do lote remessa - Preencher com '0001' para o primeiro lote do arquivo.
                '3'                                                                         + // 008 - 008 / Tipo de registro detalhe  '3' = Detalhe
                IntToStrZero(fRegLote,5)                                                    + //   9 a 13  - Número seqüencial do registro no lote - Cada título tem 2 registros (P e Q)
                'Y'                                                                         + // 014 - 014 / Cód. Segmento do registro detalhe
                Space(1)                                                                    + // 015 - 015 / Reservado (uso Banco)
                ATipoOcorrencia                                                             + // 016 - 017 / Código de movimento remessa
                ifthen( (Mensagem.Count <= 2), '2', '3' )                                   + // 018 - 018 / Identificação da impressão
                ifthen( (Mensagem.Count <= 2), '00', '' )                                   + // 019 - 020 / Reservado (uso Banco) para tipo de impressão 1 e 2
                MontarInstrucoes2                                                           + // 019 - 058 / Mensagem 5
                                                                                              // 059 - 098 / Mensagem 6
                                                                                              // 099 - 138 / Mensagem 7
                                                                                              // 139 - 178 / Mensagem 8
                                                                                              // 179 - 218 / Mensagem 9
                ifthen( (Mensagem.Count <= 2), '00' + Space(78) ,Space(22));                  // 219 - 240 / Reservado (uso Banco) para tipo de impressão 3
    end;
    Inc(fQtRegLote);
    {SEGMENTO S - FIM}
    *)

  end;//FIM with


end;


function TACBrBancoBanese.GerarRegistroTrailler240( ARemessa : TStringList ): String;
var
  wQTDTitulos: Integer;
begin

   wQTDTitulos := ARemessa.Count - 1;
   {REGISTRO TRAILER DO LOTE}
   Result:= IntToStrZero(ACBrBanco.Numero, 3)                          + //1 a 3 - Código do banco
            '0001'                                                     + //4 a 7 - Lote de Serviço - Preencher com '0001' para o primeiro lote do arquivo.
            '5'                                                        + //8     - Tipo do registro: Registro trailer do lote  '5' = Trailer de Lote
            Space(9)                                                   + //9 a 17  - Uso exclusivo FEBRABAN/CNAB
            IntToStrZero((fQtRegLote + 2 ), 6)                         + //18 a 23 - Quantidade de Registro no Lote (Registros P,Q,R,Y header e trailer do lote)
            IntToStrZero((wQTDTitulos) , 6)                            + //24 a 29 - Quantidade títulos em cobrança
            IntToStrZero( round( fValorTotalDocs * 100), 17)           + //30 a 46 - Valor dos títulos em carteiras
            PadLeft('', 6, '0')                                        + //47 a 52 - Quantidade títulos em cobrança
            PadLeft('',17, '0')                                        + //53 a 69 - Valor dos títulos em carteiras
            PadLeft('', 6, '0')                                        + //70 a 75 - Quantidade títulos em cobrança
            PadLeft('',17, '0')                                        + //76 a 92 - Quantidade de Títulos em Carteiras
            PadLeft('', 6, '0')                                        + //93 a 98 - Quantidade de Títulos em Cobrança
            PadLeft('',17, '0')                                        + //99 a 115  - Valor Total dos Títulos em Carteiras
            Space(8)                                                   + //116 a 123 - Número do Aviso de Lançamento
            Space(117)                                                 ; //124 a 240 - Uso exclusivo FEBRABAN/CNAB

   {GERAR REGISTRO TRAILER DO ARQUIVO}
   Result:= Result + #13#10 +
            IntToStrZero(ACBrBanco.Numero, 3)                          + //1 a 3 - Código do banco
            '9999'                                                     + //4 a 7 - Lote de serviço - Se registro for Trailer do Arquivo preencher com '9999'
            '9'                                                        + //8        - Tipo do registro: Registro trailer do arquivo '9' = Trailer de Arquivo
            PadLeft('',9,' ')                                          + //9 a 17   - Uso exclusivo FEBRABAN/CNAB
            PadLeft('1', 6, '0')                                       + //18 a 23  - Quantidade de lotes do arquivo - FIXO 000001
            IntToStrZero((fQtRegLote + 4) , 6)                         + //24 a 29  - Quantidade de registros do arquivo, inclusive este registro que está sendo criado agora}
            PadLeft('',6,'0')                                          + //30 a 35  - Qtde de Contas p/ Conc.(Lotes)
            PadLeft('',205,' ');                                         //36 a 240 - Uso exclusivo FEBRABAN/CNAB

   fQtRegLote := 0;
   fValorTotalDocs := 0;
end;



procedure TACBrBancoBanese.LerRetorno240(ARetorno: TStringList);
var
  ContLinha: Integer;
  Titulo   : TACBrTitulo;
  Linha, rCedente, rCNPJCPF: String;
  rAgencia, rConta,rDigitoConta: String;
  MotivoLinha, CodMotivo: Integer;
  wSeuNumero, TempData: String;
  codOcorrencia : String;
begin


  if (copy(ARetorno.Strings[0],1,3) <> '047') then
      raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                             'não é um arquivo de retorno do '+ Nome));

  rCedente := trim(Copy(ARetorno[0],73,30));
  rAgencia := trim(Copy(ARetorno[0],53,5));
  rConta   := trim(Copy(ARetorno[0],59,5));
  rDigitoConta := Copy(ARetorno[0],64,1);
  ACBrBanco.ACBrBoleto.NumeroArquivo := StrToIntDef(Copy(ARetorno[0], 158, 6), 0);

  TempData := Copy(ARetorno[1],192,2) + '/' + Copy(ARetorno[1],194,2) + '/' +
              Copy(ARetorno[1],198,2);

  if TempData <> '00/00/00' then
     ACBrBanco.ACBrBoleto.DataArquivo   := StringToDateTimeDef(TempData,0, 'DD/MM/YY');

  TempData := Copy(ARetorno[1],200,2) + '/' + Copy(ARetorno[1],202,2) + '/' +
               Copy(ARetorno[1],206,2);

  if TempData <> '00/00/00' then
     ACBrBanco.ACBrBoleto.DataCreditoLanc := StringToDateTimeDef(TempData, 0, 'DD/MM/YY');

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

  ValidarDadosRetorno(rAgencia, rConta+rDigitoConta, rCNPJCPF, True);
  with ACBrBanco.ACBrBoleto do
  begin

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

    //SEGMENTO T - Só cria após passar pelo seguimento T depois U
    if Copy(Linha,14,1)= 'T' then
       Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

    if Assigned(Titulo) then
    with Titulo do
    begin

      //SEGMENTO T
      if Copy(Linha,14,1)= 'T' then
      begin
          SeuNumero                   := Trim(copy(Linha,106,25));
          NumeroDocumento             := copy(Linha,59,11);
          codOcorrencia               := copy(Linha,16,2);
          OcorrenciaOriginal.Tipo     := CodOcorrenciaToTipo(StrToIntDef(codOcorrencia,0));
          Sacado.NomeSacado           := copy(Linha,149,40);
          //05 = Liquidação Sem Registro
          TempData := Copy(Linha,74,2) + '/' + Copy(Linha,76,2) + '/' + Copy(Linha,80,2);

          if TempData <> '00/00/00' then
            Vencimento := StringToDateTimeDef( TempData,0, 'DD/MM/YY');

          ValorDocumento       := StrToFloatDef(Copy(Linha,82,15),0)/100;
          ValorDespesaCobranca := StrToFloatDef(Copy(Linha,199,15),0)/100;
          NossoNumero          := Copy(Linha,42,15);
          Carteira             := Copy(Linha,40,2);

          if (CodOcorrencia  = '06' ) or (CodOcorrencia  = '09' ) or
             (CodOcorrencia  = '17' ) then
          begin
            CodigoLiquidacao     := Copy(Linha,214,02);
            CodigoLiquidacaoDescricao := CodigoLiquidacao_Descricao( StrToIntDef(CodigoLiquidacao,0) );
            case StrToIntDef(Copy(Linha,216,02),0) of
              01: Liquidacao.FormaPagto := 'Dinheiro';
              02: Liquidacao.FormaPagto := 'Cheque';
            else
              Liquidacao.FormaPagto := '';
            end;
          end;

          //prevenir quando o seunumero não vem informado no arquivo
          wSeuNumero := StringReplace(SeuNumero, '0','',[rfReplaceAll]);
          if (AnsiSameText(wSeuNumero, EmptyStr)) then
          begin
            SeuNumero := NossoNumero;
            NumeroDocumento := NossoNumero
          end;

          if (CodOcorrencia  = '02' ) or (CodOcorrencia  = '03' ) or
             (CodOcorrencia  = '26' ) or (CodOcorrencia  = '30' ) then
          begin
            MotivoLinha := 214;
            CodMotivo := StrToIntDef(IfThen(Copy(Linha, MotivoLinha, 2) = '00', '00', Copy(Linha, MotivoLinha, 2)), 0);

            if (CodMotivo <> 0) then
            begin
              MotivoRejeicaoComando.Add(IntToStr(CodMotivo));
              DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo, CodMotivo));
            end;
          end;

          // informações do local de pagamento
          Liquidacao.Banco      := StrToIntDef(Copy(Linha,97,3), -1);
          Liquidacao.Agencia    := Copy(Linha,100,5);
          Liquidacao.Origem     := '';

          // quando a liquidação ocorre nos canais da caixa o banco vem zero
          // então acertar
          if Liquidacao.Banco = 0 then
            Liquidacao.Banco := 104;
      end
      //SEGMENTO U
      else
      if Copy(Linha,14,1)= 'U' then
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
          ValorPago            := StrToFloatDef(Copy(Linha,78,15),0)/100;
          ValorRecebido        := StrToFloatDef(Copy(Linha,93,15),0)/100;
          ValorOutrasDespesas  := StrToFloatDef(Copy(Linha,108,15),0)/100;
          ValorOutrosCreditos  := StrToFloatDef(Copy(Linha,123,15),0)/100;
      end
      //SEGMENTO W
      else
      if Copy(Linha, 14, 1) = 'W' then
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


procedure TACBrBancoBanese.LerRetorno400(ARetorno: TStringList);
var
  ContLinha: Integer;
  Titulo   : TACBrTitulo;

  Linha,rCedente: String ;
  rCNPJCPF,rConta: String;

  CodOCorrencia: Integer;
  i, MotivoLinha : Integer;
begin

   if StrToIntDef(copy(ARetorno.Strings[0],77,3),-1) <> Numero then
      raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                             'não é um arquivo de retorno do '+ Nome));

   rCedente := trim(Copy(ARetorno[0],47,30));
   rConta   := trim(Copy(ARetorno[0],27,11));


   ACBrBanco.ACBrBoleto.DataArquivo   := StringToDateTimeDef(Copy(ARetorno[0],95,2)+'/'+
                                                             Copy(ARetorno[0],97,2)+'/'+
                                                             Copy(ARetorno[0],99,2),0, 'DD/MM/YY' );

   case StrToIntDef(Copy(ARetorno[1],2,2),0) of
      1 : rCNPJCPF:= Copy(ARetorno[1],07,11);
      2 : rCNPJCPF:= Copy(ARetorno[1],04,14);
   else
      rCNPJCPF:= Copy(ARetorno[1],4,14);
   end;

   with ACBrBanco.ACBrBoleto do
   begin
      if (not LeCedenteRetorno) and (rCNPJCPF <> OnlyNumber(Cedente.CNPJCPF)) then
         raise Exception.Create(ACBrStr('CNPJ\CPF do arquivo inválido'));

      if (not LeCedenteRetorno) and
          (rConta <> RightStr(OnlyNumber(Cedente.Conta), Length(rConta))) then
         raise Exception.Create(ACBrStr('Agencia\Conta do arquivo inválido'));

      Cedente.Nome    := rCedente;
      Cedente.CNPJCPF := rCNPJCPF;
      Cedente.Conta   := rConta;

      case StrToIntDef(Copy(ARetorno[1],2,2),0) of
         01: Cedente.TipoInscricao:= pFisica;
         else
            Cedente.TipoInscricao:= pJuridica;
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

         OcorrenciaOriginal.Tipo     := CodOcorrenciaToTipo(StrToIntDef(copy(Linha,109,2),0));

         MotivoLinha := 378;
         for i := 0 to 3 do
         begin
           //MotivoRejeicaoComando.Add(copy(Linha,MotivoLinha,2));
           MotivoRejeicaoComando.Add(IfThen(copy(Linha,MotivoLinha,2) = '  ',
                                             '00',copy(Linha,MotivoLinha,2)));

           if MotivoRejeicaoComando[i] <> '00' then
           begin
              CodOCorrencia:= StrToIntDef(MotivoRejeicaoComando[i],0) ;
              DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(
                                                OcorrenciaOriginal.Tipo,CodOCorrencia));
           end;

           MotivoLinha := MotivoLinha + 2;
         end;

         DataOcorrencia := StringToDateTimeDef( Copy(Linha,111,2)+'/'+
                                                Copy(Linha,113,2)+'/'+
                                                Copy(Linha,115,2),0, 'DD/MM/YY' );

         {Espécie do documento}
         if Trim(Copy(Linha,174,2)) = '' then
            EspecieDoc := '99'
         else
            case StrToIntDef(Copy(Linha,174,2),0) of
               01 : EspecieDoc := 'DM';
               02 : EspecieDoc := 'NP';
               03 : EspecieDoc := 'NS';
               04 : EspecieDoc := 'CS';
               05 : EspecieDoc := 'RC';
               10 : EspecieDoc := 'LC';
               11 : EspecieDoc := 'DS';
               21 : EspecieDoc := 'DM';
               22 : EspecieDoc := 'NP';
               23 : EspecieDoc := 'NS';
               24 : EspecieDoc := 'CS';
               25 : EspecieDoc := 'RC';
               30 : EspecieDoc := 'LC';
               31 : EspecieDoc := 'DS';
               39 : EspecieDoc := 'DV';
               99 : EspecieDoc := 'DV';
            else
               EspecieDoc := 'DV';
            end;

         Vencimento := StringToDateTimeDef( Copy(Linha,147,2)+'/'+
                                            Copy(Linha,149,2)+'/'+
                                            Copy(Linha,151,2),0, 'DD/MM/YY' );

         ValorDocumento       := StrToFloatDef(Copy(Linha,156,10),0)/100;
         ValorIOF             := StrToFloatDef(Copy(Linha,215,13),0)/100;
         ValorAbatimento      := StrToFloatDef(Copy(Linha,228,13),0)/100;
         ValorDesconto        := StrToFloatDef(Copy(Linha,241,13),0)/100;
         ValorMoraJuros       := StrToFloatDef(Copy(Linha,267,13),0)/100;
         ValorOutrosCreditos  := StrToFloatDef(Copy(Linha,280,13),0)/100;
         ValorRecebido        := StrToFloatDef(Copy(Linha,254,13),0)/100;
         NossoNumero          := Copy(Linha,63,8);
         Carteira             := Copy(Linha,108,1);
         ValorDespesaCobranca := StrToFloatDef(Copy(Linha,176,13),0)/100;

         if StrToIntDef(Copy(Linha,111,6),0) <> 0 then
            DataCredito:= StringToDateTimeDef( Copy(Linha,111,2)+'/'+
                                               Copy(Linha,113,2)+'/'+
                                               Copy(Linha,115,2),0, 'DD/MM/YY' );

         if StrToIntDef(Copy(Linha,111,6),0) <> 0 then
            DataBaixa := StringToDateTimeDef(Copy(Linha,111,2)+'/'+
                         Copy(Linha,113,2)+'/'+
                         Copy(Linha,115,2),0,'DD/MM/YY');

         CodigoLiquidacao := Copy(Linha,83,2); //Código Lançamento (Aviso de Movimentação)
      end;
   end;
end;

function TACBrBancoBanese.TipoOcorrenciaToDescricao(
  const TipoOcorrencia: TACBrTipoOcorrencia): String;
var
 CodOcorrencia: Integer;
begin
  Result := '';
  CodOcorrencia := StrToIntDef(TipoOcorrenciaToCod(TipoOcorrencia),0);

  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
  begin
    case CodOcorrencia of
      29: Result := '29-Ocorrência do Sacado';
      33: Result := '33-Confirmação da Alteração dos Dados do Rateio de Crédito';
      34: Result := '34-Confirmação do Cancelamento dos Dados do Rateio de Crédito';
      35: Result := '35-Confirmação do Cancelamento do Débito Automático Agendado';
    end;
  end;

  if (Result <> '') then
  begin
    Result := ACBrSTr(Result);
    Exit;
  end;

  case CodOcorrencia of
    02: Result:= '02-Entrada Confirmada';
    03: Result:= '03-Entrada Rejeitada';
    04: Result:= '04-Transferência de Carteira - Entrada';
    05: Result:= '05-Transferencia Carteira - Baixa';
    06: Result:= '06-Liquidação';
    07: Result:= '07-Confirmação do Recebimento da Instrução de Desconto';
    08: Result:= '08-Confirmação do Recebimento do Cancelamento do Desconto';
    09: Result:= '09-Baixa';
    11: Result:= '11-Título em Carteira(Em Ser)';
    12: Result:= '12-Confirmação do Recebimento de Instruções de Abatimento';
    13: Result:= '13-Confirmação do Recebimento de Instrução de Cancelamento de Abatimento';
    14: Result:= '14-Confirmação do Recebimento de Instrução de Alteração de Vencimento';
    17: Result:= '17-Liquidação após Baixa ou Liquidação de Título não Registrado';
    19: Result:= '19-Confirmação de Recebimento de Instrução de Protesto';
    20: Result:= '20-Confirmação de Recebimento de Instrução de Sustação/Cancelamento de Protesto' ;
    21: Result:= '21-Solicitação de Segunda Via de Instrumento de Protesto';
    22: Result:= '22-Segunda Via de Instrumento de Protesto Emitida pelo Cartório';
    23: Result:= '23-Remessa a Cartório';
    24: Result:= '24-Retirada de Cartório e Manutenção em Carteira';
    25: Result:= '25-Protestado e Baixado';
    26: Result:= '26-Instrução Rejeitada';
    27: Result:= '27-Confirmação do Pedido de Alteração Outros Dados';
    28: Result:= '28-Débito de Tarifas/Custas';
    30: Result:= '30-Alteração de Outros Dados Rejeitada';
    40: Result:= '40-Confirmação da Alteração do Número do Título Dado pelo Cedente';
    42: Result:= '42-Confirmação da Alteração dos Dados do Sacado';
    43: Result:= '43-Confirmação da Alteração dos Dados do Sacador Avalista';
    51: Result:= '51-Título DDA Reconhecido pelo Sacado';
    52: Result:= '52-Título DDA Não Reconhecido pelo Sacado';
    53: Result:= '53-Título DDA Recusado pela CIP';
    98: Result:= '98-Instrução de Protesto Processada';
    99: Result:= '99-Remessa Rejeitada';
  end;

  Result := ACBrSTr(Result);
end;

function TACBrBancoBanese.CodOcorrenciaToTipo(
  const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  Result := toTipoOcorrenciaNenhum;

  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
  begin
    case CodOcorrencia of
      29: Result := toRetornoOcorrenciasDoSacado;
      33: Result := toRetornoAcertoDadosRateioCredito;
      34: Result := toRetornoCancelamentoDadosRateio;
      35: Result := toRetornoDesagendamentoDebitoAutomatico;
    end;
  end;

  if (Result <> toTipoOcorrenciaNenhum) then
    Exit;

  case CodOcorrencia of
      02: Result := toRetornoRegistroConfirmado;
      03: Result := toRetornoRegistroRecusado;
      04: Result := toRetornoTransferenciaCarteiraEntrada;
      05: Result := toRetornoTransferenciaCarteiraBaixa;
      06: Result := toRetornoLiquidado;
      07: Result := toRetornoRecebimentoInstrucaoConcederDesconto;
      08: Result := toRetornoRecebimentoInstrucaoCancelarDesconto;
      09: Result := toRetornoBaixado;
      11: Result := toRetornoTituloEmSer;
      12: Result := toRetornoAbatimentoConcedido;
      13: Result := toRetornoAbatimentoCancelado;
      14: Result := toRetornoVencimentoAlterado;
      17: Result := toRetornoLiquidadoAposBaixaOuNaoRegistro;
      19: Result := toRetornoRecebimentoInstrucaoProtestar;
      20: Result := toRetornoRecebimentoInstrucaoSustarProtesto;
      21: Result := toRetornoSegundaViaInstrumentoProtesto;
      22: Result := toRetornoSegundaViaInstrumentoProtestoCartorio;
      23: Result := toRetornoEncaminhadoACartorio;
      24: Result := toRetornoInstrucaoProtestoRejeitadaSustadaOuPendente;
      25: Result := toRetornoBaixaPorProtesto;
      26: Result := toRetornoInstrucaoRejeitada;
      27: Result := toRetornoDadosAlterados;
      28: Result := toRetornoDebitoTarifas;
      30: Result := toRetornoAlteracaoDadosRejeitados;
      40: Result := toRetornoAlteracaoSeuNumero;
      42: Result := toRetornoRecebimentoInstrucaoAlterarDados;
      43: Result := toRetornoAlterarSacadorAvalista;
      51: Result := toRetornoTituloDDAReconhecidoPagador;
      52: Result := toRetornoTituloDDANaoReconhecidoPagador;
      53: Result := toRetornoTituloDDARecusadoCIP;
      98: Result := toRetornoProtestado;
      99: Result := toRetornoRemessaRejeitada;
  else
    Result := toRetornoOutrasOcorrencias;
  end;
end;

function TACBrBancoBanese.TipoOcorrenciaToCod(
  const TipoOcorrencia: TACBrTipoOcorrencia): String;
begin
  Result := '';

  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
  begin
    case TipoOcorrencia of
      toRetornoOcorrenciasDoSacado                    : Result := '29';
      toRetornoAcertoDadosRateioCredito               : Result := '33';
      toRetornoCancelamentoDadosRateio                : Result := '34';
      toRetornoDesagendamentoDebitoAutomatico         : Result := '35';
    end;
  end;

  if (Result <> '') then
    Exit;

  case TipoOcorrencia of
      toRetornoRegistroConfirmado                          : Result := '02';
      toRetornoRegistroRecusado                            : Result := '03';
      toRetornoTransferenciaCarteiraEntrada                : Result := '04';
      toRetornoTransferenciaCarteiraBaixa                  : Result := '05';
      toRetornoLiquidado                                   : Result := '06';
      toRetornoRecebimentoInstrucaoConcederDesconto        : Result := '07';
      toRetornoRecebimentoInstrucaoCancelarDesconto        : Result := '08';
      toRetornoBaixado                                     : Result := '09';
      toRetornoTituloEmSer                                 : Result := '11';
      toRetornoAbatimentoConcedido                         : Result := '12';
      toRetornoAbatimentoCancelado                         : Result := '13';
      toRetornoVencimentoAlterado                          : Result := '14';
      toRetornoLiquidadoAposBaixaOuNaoRegistro             : Result := '17';
      toRetornoRecebimentoInstrucaoProtestar               : Result := '19';
      toRetornoRecebimentoInstrucaoSustarProtesto          : Result := '20';
      toRetornoSegundaViaInstrumentoProtesto               : Result := '21';
      toRetornoSegundaViaInstrumentoProtestoCartorio       : Result := '22';
      toRetornoEncaminhadoACartorio                        : Result := '23';
      toRetornoInstrucaoProtestoRejeitadaSustadaOuPendente : Result := '24';
      toRetornoBaixaPorProtesto                            : Result := '25';
      toRetornoInstrucaoRejeitada                          : Result := '26';
      toRetornoDadosAlterados                              : Result := '27';
      toRetornoDebitoTarifas                               : Result := '28';
      toRetornoAlteracaoDadosRejeitados                    : Result := '30';
      toRetornoAlteracaoSeuNumero                          : Result := '40';
      toRetornoRecebimentoInstrucaoAlterarDados            : Result := '42';
      toRetornoAlterarSacadorAvalista                      : Result := '43';
      toRetornoTituloDDAReconhecidoPagador                 : Result := '51';
      toRetornoTituloDDANaoReconhecidoPagador              : Result := '52';
      toRetornoTituloDDARecusadoCIP                        : Result := '53';
      toRetornoProtestado                                  : Result := '98';
      toRetornoRemessaRejeitada                            : Result := '99';
  else
    Result := '02';
  end;
end;

function TACBrBancoBanese.CodMotivoRejeicaoToDescricao(
  const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: Integer): String;
begin
  case TipoOcorrencia of

      //Tabela 1
      toRetornoRegistroRecusado, toRetornoEntradaRejeitadaCarne:
      case CodMotivo  of
         01: Result := 'CODIGO DO BANCO INVALIDO';
         02: Result := 'CODIGO DO REGISTRO DETALHE INVALIDO';
         03: Result := 'CODIGO DE SEGMENTO INVALIDO';
         04: Result := 'CODIGO DE MOVIMENTO NÃO PERMITIDO PARA CARTEIRA';
         05: Result := 'CODIGO DE MOVIMENTO INVALIDO';
         06: Result := 'TIPO/NUMERO DE INCRIÇÃO DO CEDENTE INVALIDO';
         07: Result := 'AGENCIA/CONTA/DV INVALIDO';
         08: Result := 'NOSSO NUMERO INVALIDO';
         09: Result := 'NOSSO NUMERO DUPLICADO';
         10: Result := 'CARTEIRA INVALIDA';
         11: Result := 'FORMA DE CADASTRAMENTO DE TITULO INVALIDA';
         12: Result := 'TIPO DE DOCUMENTO INVALIDO';
         13: Result := 'IDENTIFICAÇÃO DA EMISSAO DE BOLETA INVALIDA';
         14: Result := 'IDENTIFICAÇÃO DA DISTRIBUIÇAO DA BOLETA INVALIDA';
         15: Result := 'CARACTERISTICAS DA COBRANÇA INCOMPATIVEIS';
         16: Result := 'DATA DE VENCIMENTO INVALIDA';
         17: Result := 'DATA DE VENCIMENTO ANTERIOR A DATA DE EMISSAO';
         18: Result := 'VENCIMENTO FORA DO PRAZO DE OPERAÇÃO';
         19: Result := 'TITULOS A CARGO DE BANCOS CORRESPODENTES COM VENCIMENTO INFERIOR A XX DIAS';
         20: Result := 'VALOR DO TITULO INVALIDO';
         21: Result := 'ESPECIE DO TITULO INVALIDO';
         22: Result := 'ESPECIE DO TITULO NAO PERMITIDO PARA A CARTEIRA';
         23: Result := 'ACEITE INVALIDO';
         24: Result := 'DATA DA EMISSÃO INVALIDA';
         25: Result := 'DATA DA EMISSÃO POSTERIOR A DATA DE ENTRADA';
         26: Result := 'CODIGO DE JUROS E MORA INVALIDO';
         27: Result := 'VALOR/TAXA JUROS INVALIDO';
         28: Result := 'CODIGO DO DESCONTO INVALIDO';
         29: Result := 'VALOR DO DESCONTO MAIOR OU IGUAL VALOR DO TITULO';
         30: Result := 'DESCONTO A CONCEDER NAO CONFERE';
         31: Result := 'CONCESSÃO DE DESCONTO - JA EXISTE DESCONTO ANTERIOR';
         32: Result := 'VALOR DO IOF INVALIDO';
         33: Result := 'VALOR DO ABATIMENTO INVALIDO';
         34: Result := 'VALOR DO ABATIMENTO MAIOR OU IGUAL AO TITULO';
         35: Result := 'VALOR A CONCEDER NAO CONFERE';
         36: Result := 'CONCESSÃO DE ABATIMENTO - JA EXISTE ABATIMENTO ANTERIOR';
         37: Result := 'CODIGO PARA PROTESTO INVALIDO';
         38: Result := 'PRAZO PARA PROTESTO INVALIDO';
         39: Result := 'PEDIDO DE PROTESTO NAO PERMITIDO PARA O TITULO';
         40: Result := 'TITULO COM ORDEM DE PROTESTO EMITIDA';
         41: Result := 'PEDIDO CANCELAMENTO/SUSTAÇÃO PARA TITULO SEM INSTRUÇÃO DE PROTESTO';
         42: Result := 'CODIGO PARA BAIXA/DEVOLUÇÃO INVALIDO';
         43: Result := 'PRAZO PARA BAIXA/DEVOLUÇÃO INVALIDO';
         44: Result := 'CODIGO DA MOEDA INVALIDO';
         45: Result := 'NOME DO SACADO NAO INFORMADO';
         46: Result := 'TIPO/NUMERO DE INSCRIÇÃO DO SACADO INVALIDO';
         47: Result := 'ENDEREÇO DO SACADO NÃO INFORMADO';
         48: Result := 'CEP INVALIDO';
         49: Result := 'CEP SEM PRAÇA DE COBRANÇA (NAO LOCALIZADO)';
         50: Result := 'CEP REFERENTE A UM BANCO CORRESPODENTE';
         51: Result := 'CEP INCOMPATIVEL COM A UNIDADE DA FEDERAÇÃO';
         52: Result := 'UNIDADE DA FEDERAÇÃO INVALIDA';
         53: Result := 'TIPO/NUMERO INSCRIÇÃO DO SACADOR/AVALISTA INVALIDO';
         54: Result := 'SACADOR AVALISTA NAO INFORMADO';
         55: Result := 'NOSSO NUMERO NO BANCO CORRESPODENTE NAO INFORMADO';
         56: Result := 'CODIGO DO BANCO CORRESPODENTE NAO INFORMADO';
         57: Result := 'CODIGO DA MULTA INVALIDO';
         58: Result := 'DATA DA MULTA INVALIDA';
         59: Result := 'VALOR PERCENTUAL DA MULTA INVALIDA';
         60: Result := 'MOVIMENTO PARA TITULO NAO CADASTRADO';
         61: Result := 'ALTERAÇÃO DA AGÊNCIA COBRADORA DV INVALIDO';
         62: Result := 'TIPO IMPRESSÃO INVALIDA';
         63: Result := 'ENTRADA TITULO JA CADASTRADO';
         64: Result := 'NUMERO LINHA INVALIDO';
         65: Result := 'CODIGO BANCO PARA DEBITO INVALIDO';
         66: Result := 'AGENCIA / CC /DV PARA DEBITO INVALIDO';
         67: Result := 'DADOS PARA DEBITO INCOMPATIVEL COM A IDENTIFICACAO DA EMISSAO DO BOLETO';
         68: Result := 'DEBITO AUTOMATICO AGENDADO';
      else
         Result := IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

      toRetornoTarifaOcorrencias:
      case CodMotivo of
         01: Result := 'TARIFAS DE EXTRATO DE POSIÇÃO';
         02: Result := 'TAFIFA DE MANUTENÇÃO DE TITULO VENCIDA';
         03: Result := 'TARIFA DE SUSTAÇÃO';
         04: Result := 'TARIFA DE PROTESTO';
         05: Result := 'TARIFA DE OUTRAS INSTRUÇOES';
         06: Result := 'TARIFA DE OUTRAS OCORRENCIAS';
         07: Result := 'TARIFA DE ENVIO DE DUPLICATA AO SACADO';
         08: Result := 'CUSTAS DE PROTESTO';
         09: Result := 'CUSTAS DE SUSTAÇÃO DE PROTESTO';
         10: Result := 'CUSTAS DE CARTÓRIO DISTRIBUIDOR';
         11: Result := 'CUSTAS DE EDITAL';
         12: Result := 'TARIFA SOBRE DEVOLUCAO E TITULO VENCIDO';
         13: Result := 'TARIFA SOBRE REGISTRO COBRADO NA BAIXA';
         14: Result := 'TARIFA SOBRE REAPRESENTAÇÃO AUTOMATICA';
         15: Result := 'TARIFA SOBRE RATEIO DE CRÉDITO';
         16: Result := 'TARIFA SOBRE INFORMAÇÃO VIA FAX';
         17: Result := 'TARIFA SOBRE PRORROGAÇÃO DE VENCIMENTO';
         18: Result := 'TARIFA SOBRE ALTERAÇÃO DE ABATIMENTO/DESCONTO';
         19: Result := 'TARIFA SOBRE ARQUIVO MENSAL';
         20: Result := 'TARIFA EMISSÃO BOLETO EMITIDO PELO BANCO';

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

  Result := ACBrSTr(Result);
end;
end.


