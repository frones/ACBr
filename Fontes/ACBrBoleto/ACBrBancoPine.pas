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

unit ACBrBancoPine;

interface

uses
  Classes, Contnrs, SysUtils, ACBrBoleto, ACBrBoletoConversao;

type

  { TACBrBancoPine }

  TACBrBancoPine = class(TACBrBancoClass)
  private
    function TipoOcorrenciaRemessaToCod(const TipoOcorrencia: TACBrTipoOcorrencia): String;
    function EspecieDocToCod(const Especie: String): String;
    function CodToEspecie(const CodEspecie: Integer): String;
    function MontarPosicoesSacador(const ACBrTitulo:TACBrTitulo; const Instrucao94: Boolean): String;
    function MontarRegistroMensagens(const ACBrTitulo:TACBrTitulo; const Instrucao94: Boolean): String;
    function MontarRegistroSacador(const ACBrTitulo:TACBrTitulo): String;
    procedure GerarRegistrosNFe(ACBrTitulo : TACBrTitulo; aRemessa: TStringList);
  protected
    fCorrespondente: Boolean;
    function CalcularNossoNumero(const ACBrTitulo:TACBrTitulo): String; virtual;
    function CalcularCarteira(const ACBrTitulo:TACBrTitulo): String; virtual;
    function LerNossoNumero(const aLinha:String; ACBrTitulo : TACBrTitulo ):String;  virtual;
    function GetLocalPagamento: string;  override;
  public
    Constructor create(AOwner: TACBrBanco);
    function CalcularDigitoVerificador(const ACBrTitulo:TACBrTitulo): String; override;
    function MontarCodigoBarras(const ACBrTitulo : TACBrTitulo): String; override;
    function MontarCampoNossoNumero(const ACBrTitulo :TACBrTitulo): String; override;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String; override;
    procedure GerarRegistroHeader400(NumeroRemessa : Integer; ARemessa:TStringList); override;
    procedure GerarRegistroTransacao400(ACBrTitulo : TACBrTitulo; aRemessa: TStringList); override;
    procedure GerarRegistroTrailler400(ARemessa:TStringList);  override;
    Procedure LerRetorno400(ARetorno:TStringList); override;

    function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia) : String; override;
    function CodOcorrenciaToTipo(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
    function TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia):String; override;
    function CodMotivoRejeicaoToDescricao(const TipoOcorrencia:TACBrTipoOcorrencia; CodMotivo:Integer): String; override;
  end;

implementation

uses {$IFDEF COMPILER6_UP} dateutils {$ELSE} ACBrD5 {$ENDIF},
  StrUtils, ACBrUtil.Base, ACBrValidador, ACBrUtil.Strings, ACBrUtil.DateTime;

{ TACBrBancoPine }

function TACBrBancoPine.LerNossoNumero(const aLinha: String; ACBrTitulo : TACBrTitulo): String;
begin
   ACBrTitulo.NossoNumero:= Copy(aLinha, 63, 10);
end;

function TACBrBancoPine.GetLocalPagamento: string;
begin
   if fCorrespondente then
      Result:= ACBrStr('Pagável preferencialmente na Rede Bradesco ou Bradesco Expresso')
   else
      Result:= ACBrStr('Canais eletrônicos, agências ou correspondentes bancários de todo o BRASIL');

end;

function TACBrBancoPine.TipoOcorrenciaRemessaToCod(const TipoOcorrencia: TACBrTipoOcorrencia): String;
begin
   Result := '';

   case TipoOcorrencia of
     toRemessaRegistrar          : Result := '01';
     toRemessaBaixar             : Result := '02';
     toRemessaConcederAbatimento : Result := '04';
     toRemessaCancelarAbatimento : Result := '05';
     toRemessaAlterarVencimento  : Result := '06';
     toRemessaProtestar          : Result := '09';
     toRemessaNaoProtestar       : Result := '10';
     toRemessaSustarProtesto     : Result := '18';
     toRemessaAlterarValorTitulo : Result := '47';
  else
    Result:= '01';
  end;
end;

function TACBrBancoPine.EspecieDocToCod(const Especie: String): String;
var
  wEspecie: String;
begin
   wEspecie:= Trim(Especie);

   case AnsiIndexStr(wEspecie, ['DM', 'NP', 'CH', 'LC', 'RE', 'AS', 'DS', 'CD',
                                'OU']) of

     00: Result:= '01';
     01: Result:= '02';
     02: Result:= '03';
     03: Result:= '04';
     04: Result:= '05';
     05: Result:= '08';
     06: Result:= '12';
     07: Result:= '31';
     08: Result:= '99';
   else
      Result:= wEspecie;
   end;
end;

function TACBrBancoPine.CodToEspecie(const CodEspecie: Integer): String;
begin
   case CodEspecie of
      01: Result:= 'DM';
      02: Result:= 'NP';
      03: Result:= 'CH';
      04: Result:= 'LC';
      05: Result:= 'RE';
      08: Result:= 'AS';
      12: Result:= 'DS';
      31: Result:= 'CD';
   else
      Result:= 'OU';
   end;
end;

function TACBrBancoPine.MontarPosicoesSacador(const ACBrTitulo: TACBrTitulo;
  const Instrucao94: Boolean ): String;
begin
  if Instrucao94 then
  begin
     if ACBrTitulo.Mensagem.Count > 0 then
        Result:= PadRight(ACBrTitulo.Mensagem[0], 40)
     else
        Result:= StringOfChar(' ', 40);
  end
  else
     Result:= PadRight(ACBrTitulo.Sacado.SacadoAvalista.NomeAvalista,40);
end;

function TACBrBancoPine.MontarRegistroMensagens(const ACBrTitulo: TACBrTitulo;
  const Instrucao94: Boolean): String;
var
  I: Integer;
begin
   Result:= '';

   if (Instrucao94) then
      I:= 1
   else
      I:= 0;

   if (Instrucao94) and (I =  ACBrTitulo.Mensagem.Count) then
      exit;

   Result:= '2' + '0' ;
   while (I < 5) and  (I < ACBrTitulo.Mensagem.Count) do
   begin
      Result := Result + PadRight(Copy(ACBrTitulo.Mensagem[I], 1, 69), 69);
      Inc(I);
   end;
   Result:= PadRight(Result, 347)  + StringOfChar(' ', 47);
end;

function TACBrBancoPine.MontarRegistroSacador(const ACBrTitulo: TACBrTitulo
  ): String;
var
  wTipoSacador: String;
begin
  if  ACBrTitulo.Sacado.SacadoAvalista.Pessoa = pFisica then
     wTipoSacador := '01'
  else
     wTipoSacador := '02';

  Result:= '5'       +
           StringOfChar(' ',120)  +
           wTipoSacador           +
           PadLeft(OnlyNumber(ACBrTitulo.Sacado.SacadoAvalista.CNPJCPF), 14, '0') +
           PadRight(ACBrTitulo.Sacado.SacadoAvalista.Logradouro + ' '             +
                    ACBrTitulo.Sacado.SacadoAvalista.Numero + ' '                 +
                    ACBrTitulo.Sacado.SacadoAvalista.Complemento,40)              +
           PadRight(ACBrTitulo.Sacado.SacadoAvalista.Bairro,12)                   +
           PadLeft(ACBrTitulo.Sacado.SacadoAvalista.CEP, 8, '0')                  +
           PadRight(ACBrTitulo.Sacado.SacadoAvalista.Cidade,15)                   +
           PadRight(ACBrTitulo.Sacado.SacadoAvalista.UF,2)                        +
           StringOfChar(' ', 180);
end;

function TACBrBancoPine.CalcularNossoNumero(const ACBrTitulo: TACBrTitulo
  ): String;
begin
  if ACBrTitulo.ACBrBoleto.Cedente.ResponEmissao = tbCliEmite then
     Result:= ACBrTitulo.NossoNumero + CalcularDigitoVerificador(ACBrTitulo)
  else
     Result:= StringOfChar('0',11);

  Result:= Result +
           StringOfChar(' ', 16 ); {74 a  89- Nosso numero e carteira no correspondente}
end;

function TACBrBancoPine.CalcularCarteira(const ACBrTitulo: TACBrTitulo): String;
begin
   if ACBrBanco.ACBrBoleto.Cedente.ResponEmissao = tbCliEmite then
      Result:= 'D'
   else
      Result:= '1';
end;

procedure TACBrBancoPine.GerarRegistrosNFe(ACBrTitulo: TACBrTitulo;
  aRemessa: TStringList);
var
  wQtdRegNFes, J, I, wQtdNFeNaLinha: Integer;
  wLinha, NFeSemDados: String;
  Continua: Boolean;
begin
   NFeSemDados:= StringOfChar(' ',15) + StringOfChar('0', 65);
   wQtdRegNFes:= trunc(ACBrTitulo.ListaDadosNFe.Count / 3);

   if (ACBrTitulo.ListaDadosNFe.Count mod 3) <> 0 then
      Inc(wQtdRegNFes);

   J:= 0;
   I:= 0;
   repeat
   begin
      Continua:=  true;

      wLinha:= '4';
      wQtdNFeNaLinha:= 0;
      while (Continua) and (J < ACBrTitulo.ListaDadosNFe.Count) do
      begin
         wLinha:= wLinha +
                  PadRight(ACBrTitulo.ListaDadosNFe[J].NumNFe,15) +
                  IntToStrZero( round(ACBrTitulo.ListaDadosNFe[J].ValorNFe  * 100 ), 13) +
                  FormatDateTime('ddmmyyyy',ACBrTitulo.ListaDadosNFe[J].EmissaoNFe)      +
                  PadLeft(ACBrTitulo.ListaDadosNFe[J].ChaveNFe, 44, '0');

         Inc(J);
         Inc(wQtdNFeNaLinha);
         Continua:= (J mod 3) <> 0 ;
      end;

      if wQtdNFeNaLinha < 3 then
      begin
         wLinha:= wLinha + NFeSemDados;
         if wQtdNFeNaLinha < 2 then
            wLinha:= wLinha + NFeSemDados;
      end;

      wLinha:= PadRight(wLinha,241) + StringOfChar(' ', 153) +
               IntToStrZero(aRemessa.Count + 1, 6);

      aRemessa.Add(wLinha);
      Inc(I);
   end;
   until (I = wQtdRegNFes) ;
end;

constructor TACBrBancoPine.create(AOwner: TACBrBanco);
begin
   inherited create(AOwner);
   fpNome                   := 'Pine';
   fpNumero                 := 643;
   fpDigito                 := 2;
   fpTamanhoMaximoNossoNum  := 10;
   fpTamanhoAgencia         := 4;
   fpTamanhoConta           := 6;
   fpTamanhoCarteira        := 3;
   fCorrespondente          := False;
end;

function TACBrBancoPine.CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo ): String;
var
  Docto: String;
begin
   Result := '0';
   Docto := ACBrBanco.ACBrBoleto.Cedente.Agencia +
            ACBrTitulo.Carteira +
            ACBrTitulo.NossoNumero;


   Modulo.MultiplicadorInicial := 1;
   Modulo.MultiplicadorFinal   := 2;
   Modulo.MultiplicadorAtual   := 2;
   Modulo.FormulaDigito := frModulo10;
   Modulo.Documento:= Docto;
   Modulo.Calcular;

   Result := IntToStr(Modulo.DigitoFinal);
end;

function TACBrBancoPine.MontarCodigoBarras ( const ACBrTitulo: TACBrTitulo) : String;
var
  CodigoBarras, FatorVencimento, DigitoCodBarras:String;
begin
   with ACBrTitulo.ACBrBoleto do
   begin
      FatorVencimento := CalcularFatorVencimento(ACBrTitulo.Vencimento);

      CodigoBarras := IntToStr( Numero )+'9'+ FatorVencimento +
                      IntToStrZero(Round(ACBrTitulo.ValorDocumento*100),10) +
                      PadLeft(OnlyNumber(Cedente.Agencia), fpTamanhoAgencia, '0') +
                      ACBrTitulo.Carteira +
                      PadLeft(RightStr(Cedente.Operacao,7),7,'0') +
                      ACBrTitulo.NossoNumero +
                      CalcularDigitoVerificador(ACBrTitulo);

      DigitoCodBarras := CalcularDigitoCodigoBarras(CodigoBarras);
   end;
   Result:= IntToStr(Numero) + '9'+ DigitoCodBarras + Copy(CodigoBarras,5,39);

end;

function TACBrBancoPine.MontarCampoNossoNumero (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
   Result:=  ACBrTitulo.NossoNumero + '-' + CalcularDigitoVerificador(ACBrTitulo);
end;

function TACBrBancoPine.MontarCampoCodigoCedente (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
   Result:= ACBrTitulo.ACBrBoleto.Cedente.Agencia + '/' +
            ACBrTitulo.ACBrBoleto.Cedente.CodigoCedente;
end;

procedure TACBrBancoPine.GerarRegistroHeader400(NumeroRemessa : Integer; ARemessa:TStringList);
var
  wLinha: String;
begin
   wLinha:= '0'                                                     +  {1- ID do Registro }
            '1'                                                     +  {2- ID do Arquivo( 1 - Remessa) }
            'REMESSA'                                               +  {3 a 9 - Literal de Remessa }
            '01'                                                    +  {10 a 11 - Código do Tipo de Serviço }
            PadRight('COBRANCA', 15)                                +  {12 a 26 - Descrição do tipo de serviço }
            PadRight(ACBrBanco.ACBrBoleto.Cedente.CodigoCedente, 20)   +  {27 a 46 - Codigo da Empresa no Banco }
            PadRight(Copy(ACBrBanco.ACBrBoleto.Cedente.Nome,1, 30), 30)+  {47 a 76 - Nome da Empresa }
            '643'                                                   +  {77 a 79 - Código do Banco(643) }
            PadRight('BANCO PINE', 15)                              +  {80 a 94 - Nome do Banco(Pine ) }
            FormatDateTime('ddmmyy',Now)                            +  {95 a 100 - Data de geração do arquivo}
            Space(294)                                              +  {101 a 394 - brancos }
            IntToStrZero(1, 6);                                       {395 a 400 - Contador }

   ARemessa.Text:= UpperCase(wLinha);

end;

procedure TACBrBancoPine.GerarRegistroTransacao400(ACBrTitulo :TACBrTitulo; aRemessa: TStringList);
var
  wLinha, wTipoInscricao, wNossoNumero, wMulta, wDiasMulta, wSeuNumero: String;
  wDiasProtesto, wCNPJCedente, wDataDesconto: String;
  wInstrucao94: Boolean;
  wTipoMulta: Char;
begin
   case ACBrBanco.ACBrBoleto.Cedente.TipoInscricao of
     pFisica  : wTipoInscricao:= '01';
     pJuridica: wTipoInscricao:= '02';
   else
     wTipoInscricao:= '02';
   end;

   wCNPJCedente:= OnlyNumber(ACBrBanco.ACBrBoleto.Cedente.CNPJCPF);


   wNossoNumero:= CalcularNossoNumero(ACBrTitulo);

   wDiasMulta:= StringOfChar('0',2);
   if ACBrTitulo.PercentualMulta > 0 then
   begin
      if ACBrTitulo.CodigoMulta = cmValorFixo then
      begin
         wTipoMulta:= '1';
         wMulta    := IntToStrZero(round(ACBrTitulo.PercentualMulta * 100), 13);
      end
      else
      begin
         wTipoMulta:= '2';
         wMulta    := IntToStrZero(round(ACBrTitulo.PercentualMulta * 10000), 13);
      end;

      if ACBrTitulo.DataMulta > 0 then
         wDiasMulta:= IntToStrZero(DaysBetween(ACBrTitulo.Vencimento, ACBrTitulo.DataMulta),2);
   end
   else
   begin
      wTipoMulta:= '0';
      wMulta    := StringOfChar('0',13);
   end;

   wSeuNumero:= PadRight(IfThen(ACBrTitulo.SeuNumero <> '', ACBrTitulo.SeuNumero,
                         ACBrTitulo.NumeroDocumento),10);

   if ACBrTitulo.DataProtesto > 0 then
      wDiasProtesto:=  IntToStrZero(DaysBetween(ACBrTitulo.Vencimento,
                                     ACBrTitulo.DataProtesto),2)
   else
      wDiasProtesto:= '00';

   if (ACBrTitulo.ValorDesconto > 0) and (ACBrTitulo.DataDesconto > 0) then
      wDataDesconto:= FormatDateTime( 'ddmmyy', ACBrTitulo.DataDesconto)
   else
      wDataDesconto:= StringOfChar('0',6);

   wInstrucao94:=  (ACBrTitulo.Instrucao1 = '94') or (ACBrTitulo.Instrucao2 = '94');

   wLinha:= '1'                                                                  + {1- ID Registro }
            wTipoInscricao                                                       + {2 a 3 - Id do Tipo de Inscrição}
            PadLeft(wCNPJCedente, 14, '0')                                 + {4 a 17 - CNPJ/CPF da Empresa}
            PadRight(ACBrBanco.ACBrBoleto.Cedente.CodigoCedente, 20)             + {18 a 37 - Codigo da Empresa no Banco }
            PadRight(ACBrTitulo.NumeroDocumento,25)                              + {38 a 62 - Id do Titulo na Empresa}
            wNossoNumero                                                   + {63 a 73 - Nosso Numero + DV / 74 a 89 Nosso Numero + Carteira correspondente}
            wTipoMulta                                                     + {90 - Código de Multa}
            wMulta                                                         + {91 a 103 - Valor/Percentual de Multa}
            wDiasMulta                                                     + {104 a 105 - Dias para Multa}
            StringOfChar(' ',2)                                            + {106 a 107 - Uso do banco}
            CalcularCarteira(ACBrTitulo)                                   + {108 - Código da Carteira - Somente Pine}
            TipoOcorrenciaRemessaToCod(ACBrTitulo.OcorrenciaOriginal.Tipo) + {109 a 110 - Código de Ocorrencia}
            wSeuNumero                                                     + {111 a 120 - Seu Numero}
            FormatDateTime( 'ddmmyy', ACBrTitulo.Vencimento)               + {121 a 126 - Data Vencimento}
            IntToStrZero( Round( ACBrTitulo.ValorDocumento * 100 ), 13)    + {127 a 139 - Valo Titulo}
            IntToStr( ACBrTitulo.ACBrBoleto.Banco.Numero)                  + {140 a 142 - Numero do banco}
            StringOfChar('0',5)                                            + {143 a 147 - Zeros}
            EspecieDocToCod(ACBrTitulo.EspecieDoc)                         + {148 a 149 - Especie do Documento}
            IfThen(ACBrTitulo.Aceite = atSim, 'A', 'N')                    + {150 - Aceite}
            FormatDateTime( 'ddmmyy', ACBrTitulo.DataDocumento)            + {151 a 156 - Data Emissao}
            PadLeft(trim(ACBrStr(ACBrTitulo.Instrucao1)), 2, '0')          + {157 a 158 - 1ª INSTRUÇÃO}
            PadLeft(trim(ACBrStr(ACBrTitulo.Instrucao2)), 2, '0')          + {159 a 160 - 2ª INSTRUÇÃO}
            IntToStrZero( round(ACBrTitulo.ValorMoraJuros * 100 ), 13)     + {161 a 173 - Valor a ser cobrado por dia de atraso}
            wDataDesconto                                                  + {174 a 179 - Data limite para desconto}
            IntToStrZero( round(ACBrTitulo.ValorDesconto * 100 ), 13)      + {180 a 192 - Valor de desconto a ser concedido para pagto antecipado}
            IntToStrZero( round(ACBrTitulo.ValorIOF * 100 ), 13)           + {193 a 205 - Valor IOF}
            IntToStrZero( round(ACBrTitulo.ValorAbatimento * 100 ), 13)    + {206 a 218 - Valor de abatimento a ser concedido}
            IfThen(ACBrTitulo.Sacado.Pessoa = pFisica, '01', '02')         + {219 a 220 - Tipo Inscrição do Pagaador}
            PadLeft(OnlyNumber(ACBrTitulo.Sacado.CNPJCPF), 14, '0')        + {221 a 234 - CPF/CNPJ do pagador}
            PadRight(ACBrTitulo.Sacado.NomeSacado, 30)                     + {235 a 264 - Nome do pagador}
            StringOfChar(' ', 10)                                          + {265 a 274 - Brancos}
            PadRight(ACBrTitulo.Sacado.Logradouro + ' ' +
                     ACBrTitulo.Sacado.Numero +  ' ' +
                     ACBrTitulo.Sacado.Complemento, 40)                    + {275 a 314 - Rua}
            PadRight(ACBrTitulo.Sacado.Bairro, 12)                         + {315 a 326 - Bairro}
            PadLeft(OnlyNumber(ACBrTitulo.Sacado.CEP), 8)                  + {327 a 334 - CEP}
            PadRight(ACBrTitulo.Sacado.Cidade, 15)                         + {335 a 349 - Cidade}
            ACBrTitulo.Sacado.UF                                           + {350 a 351 - UF}
            MontarPosicoesSacador(ACBrTitulo, wInstrucao94)                + {352 a 391 - Dados Sacador ou Mensagens}
            wDiasProtesto + '0'                                            + {392 a 393 - Dias para protesto + Moeda}
            IntToStrZero(aRemessa.Count + 1, 6);                             {394 a 400 - Sequencial do Registro no arquivo}


   aRemessa.Add(UpperCase(wLinha));

   if ACBrTitulo.Mensagem.Count > 0 then
   begin
      wLinha:= MontarRegistroMensagens(ACBrTitulo, wInstrucao94) + IntToStrZero(aRemessa.Count + 1, 6);
      if wLinha <> '' then
         aRemessa.Add(UpperCase(wLinha));
   end;

   if (not fCorrespondente) and (trim(ACBrTitulo.Sacado.SacadoAvalista.NomeAvalista) <> '') then
   begin
      wLinha:= MontarRegistroSacador(ACBrTitulo) + IntToStrZero(aRemessa.Count + 1,6);
      aRemessa.Add(UpperCase(wLinha));
   end;

   if ACBrTitulo.ListaDadosNFe.Count > 0 then
      GerarRegistrosNFe(ACBrTitulo, aRemessa);

end;

procedure TACBrBancoPine.GerarRegistroTrailler400( ARemessa:TStringList );
var
  wLinha: String;
begin
   wLinha := '9' + Space(393) + IntToStrZero( ARemessa.Count + 1, 6);

   ARemessa.Add(wLinha);
end;

procedure TACBrBancoPine.LerRetorno400(ARetorno: TStringList);
var
  Titulo : TACBrTitulo;
  ContLinha, CodOcorrencia  :Integer;
  CodMotivo, i, MotivoLinha :Integer;
  Linha, rCedente, rCNPJCPF :String;
  rCodEmpresa               :String;
begin
   if (StrToIntDef(copy(ARetorno.Strings[0],77,3),-1) <> 643) then
      raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                             'não é um arquivo de retorno do Banco Pine'));

   rCodEmpresa:= trim(Copy(ARetorno[0],27,20));
   rCedente   := trim(Copy(ARetorno[0],47,30));

   ACBrBanco.ACBrBoleto.NumeroArquivo := StrToIntDef(Copy(ARetorno[0],109,5),0);

   ACBrBanco.ACBrBoleto.DataArquivo := StringToDateTimeDef(Copy(ARetorno[0],95,2)+'/'+
                                                           Copy(ARetorno[0],97,2)+'/'+
                                                           Copy(ARetorno[0],99,2),0, 'DD/MM/YY' );

   case StrToIntDef(Copy(ARetorno[1],2,2),0) of
      1: rCNPJCPF := Copy(ARetorno[1],7,11);
      2: rCNPJCPF := Copy(ARetorno[1],4,14);
   else
     rCNPJCPF := Copy(ARetorno[1],4,14);
   end;

   ValidarDadosRetorno('', '',rCNPJCPF);
   with ACBrBanco.ACBrBoleto do
   begin
      if (not LeCedenteRetorno) and (rCodEmpresa <> trim(Cedente.CodigoCedente)) then
         raise Exception.Create(ACBrStr('Código da Empresa do arquivo inválido ' + rCodEmpresa + ' c:' + Cedente.CodigoCedente));

      case StrToIntDef(Copy(ARetorno[1],2,2),0) of
         1: Cedente.TipoInscricao:= pFisica;
         2: Cedente.TipoInscricao:= pJuridica;
      else
         Cedente.TipoInscricao := pJuridica;
      end;

      if LeCedenteRetorno then
      begin
         Cedente.CNPJCPF      := rCNPJCPF;
         Cedente.CodigoCedente:= rCodEmpresa;
         Cedente.Nome         := rCedente;
      end;

      ACBrBanco.ACBrBoleto.ListadeBoletos.Clear;
   end;

   for ContLinha := 1 to ARetorno.Count - 2 do
   begin
      Linha := ARetorno[ContLinha] ;

      if Copy(Linha,1,1)<> '1' then
         Continue;

      Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

      Titulo.NumeroDocumento := copy(Linha, 38, 25);
      Titulo.SeuNumero       := copy(Linha, 117, 10);
      Titulo.Carteira        := Copy(Linha, 83, 3);
      Titulo.EspecieDoc      := CodToEspecie(StrToIntDef(Copy(Linha, 174, 2),0));
      LerNossoNumero(Linha, Titulo);

      case  Linha[108] of
        '1': Titulo.CaracTitulo:= tcSimples;
        '2': Titulo.CaracTitulo:= tcVinculada;
        '3': Titulo.CaracTitulo:= tcCaucionada;
      else
        Titulo.CaracTitulo:= tcDireta;
      end;

      Titulo.ValorDocumento  := StrToFloatDef(Copy(Linha, 153, 13), 0) / 100;
      Titulo.ValorAbatimento := StrToFloatDef(Copy(Linha, 228, 13), 0) / 100;
      Titulo.ValorDesconto   := StrToFloatDef(Copy(Linha, 241, 13), 0) / 100;
      Titulo.ValorPago       := StrToFloatDef(Copy(Linha, 254, 13), 0) / 100;
      Titulo.ValorMoraJuros  := StrToFloatDef(Copy(Linha, 267, 13), 0) / 100;
      Titulo.ValorRecebido   := Titulo.ValorPago + Titulo.ValorMoraJuros;

      if Copy(Linha,147,2) <> '00' then
         Titulo.Vencimento := StringToDateTimeDef( Copy(Linha, 147, 2)+ '/' +
                                                   Copy(Linha, 149, 2)+ '/' +
                                                   Copy(Linha, 151, 2), 0, 'DD/MM/YY' );


      if Copy(Linha,386,2) <> '00' then
         Titulo.DataCredito := StringToDateTimeDef( Copy(Linha, 386, 2)+ '/' +
                                                    Copy(Linha, 388, 2)+ '/' +
                                                    Copy(Linha, 390, 2), 0, 'DD/MM/YY' );


      CodOcorrencia := StrToIntDef(copy(Linha, 109, 2), 0);
      Titulo.OcorrenciaOriginal.Tipo := CodOcorrenciaToTipo(CodOcorrencia);
      Titulo.DataOcorrencia          := StringToDateTimeDef( Copy(Linha, 111, 2)+ '/' +
                                                             Copy(Linha, 113, 2)+ '/' +
                                                             Copy(Linha, 115, 2), 0, 'DD/MM/YY' );

      MotivoLinha := 378;
      if(CodOcorrencia in [03, 15, 16])then
      begin
         for i := 0 to 4 do
         begin
            CodMotivo := StrToIntDef(copy(Linha,MotivoLinha,2),0);
            if CodMotivo > 0 then
            begin
               Titulo.MotivoRejeicaoComando.Add(copy(Linha,MotivoLinha,2));
               Titulo.DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(Titulo.OcorrenciaOriginal.Tipo,CodMotivo));
            end;

            MotivoLinha := MotivoLinha + 2; //Incrementa a coluna dos motivos
         end;
      end;
   end;
end;

function TACBrBancoPine.TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String;
var
  CodOcorrencia: Integer;
begin
   Result := '';
   CodOcorrencia := StrToIntDef(TipoOcorrenciaToCod(TipoOcorrencia),0);

   // Ver oque seria 01 - confirma entrada CIP  /05 Campo Livre Alterado
   // Tarifa Sobre Baixas – Mês Anterior /98 Tarifa Sobre Entradas – Mês Anterior

   case CodOcorrencia of
     01: Result:= 'Confirma Entrada Título na CIP';
     02: Result:= 'Entrada Confirmada';
     03: Result:= 'Entrada Rejeitada';
     05: Result:= 'Campo Livre Alterado';
     06: Result:= 'Liquidação Normal';
     08: Result:= 'Liquidação em Cartório';
     09: Result:= 'Baixa Automática';
     10: Result:= 'Baixa por ter sido liquidado';
     12: Result:= 'Confirma Abatimento';
     13: Result:= 'Abatimento Cancelado';
     14: Result:= 'Vencimento Alterado';
     15: Result:= 'Baixa Rejeitada';
     16: Result:= 'Instrução Rejeitada';
     19: Result:= 'Confirma Recebimento de Ordem de Protesto';
     20: Result:= 'Confirma Recebimento de Ordem de Sustação';
     22: Result:= 'Seu número alterado';
     23: Result:= 'Título enviado para cartório';
     24: Result:= 'Confirma recebimento de ordem de não protestar';
     28: Result:= 'Débito de Tarifas/Custas – Correspondentes';
     40: Result:= 'Tarifa de Entrada (debitada na Liquidação)';
     43: Result:= 'Baixado por ter sido protestado';
     96: Result:= 'Tarifa Sobre Instruções – Mês anterior';
     97: Result:= 'Tarifa Sobre Baixas – Mês Anterior';
     98: Result:= 'Tarifa Sobre Entradas – Mês Anterior';
   end;

   Result := ACBrSTr(Result);
end;

function TACBrBancoPine.CodOcorrenciaToTipo(const CodOcorrencia:
   Integer ) : TACBrTipoOcorrencia;
begin
   Result := toTipoOcorrenciaNenhum;

   // Ver oque seria 01 - confirma entrada CIP  /05 Campo Livre Alterado
   // Tarifa Sobre Baixas – Mês Anterior /98 Tarifa Sobre Entradas – Mês Anterior

   case CodOcorrencia of
      1,2: Result:= toRetornoRegistroConfirmado;
      3: Result:= toRetornoRegistroRecusado;
      6: Result:= toRetornoLiquidado;
      8: Result:= toRetornoLiquidadoEmCartorio;
      9: Result:= toRetornoBaixaAutomatica;
      10:Result:= toRetornoBaixaPorTerSidoLiquidado;
      12:Result:= toRetornoAbatimentoConcedido;
      13:Result:= toRetornoAbatimentoCancelado;
      14:Result:= toRetornoVencimentoAlterado;
      15:Result:= toRetornoBaixaRejeitada;
      16:Result:= toRetornoInstrucaoRejeitada;
      19:Result:= toRetornoRecebimentoInstrucaoProtestar;
      20:Result:= toRetornoRecebimentoInstrucaoSustarProtesto;
      22:Result:= toRetornoAlteracaoSeuNumero;
      23:Result:= toRetornoEncaminhadoACartorio;
      24:Result:=toRetornoRecebimentoInstrucaoNaoProtestar;
      28:Result:= toRetornoTarifaMensalLiquidacoesBancosCorrespCarteira;
      40:Result:= toRetornoTarifaDeRelacaoDasLiquidacoes;
      43:Result:= toRetornoBaixaPorProtesto;
      96:Result:= toRetornoTarifaInstrucao;
      97:Result:= toRetornoTarifaMensalBaixasCarteira;
   end;

end;

function TACBrBancoPine.TipoOcorrenciaToCod(
  const TipoOcorrencia: TACBrTipoOcorrencia): String;
begin
   Result := '';

   // Ver oque seria 01 - confirma entrada CIP  /05 Campo Livre Alterado
   // Tarifa Sobre Baixas – Mês Anterior /98 Tarifa Sobre Entradas – Mês Anterior

   case TipoOcorrencia of
      toRetornoRegistroConfirmado                 : Result := '02';
      toRetornoRegistroRecusado                   : Result := '03';
      toRetornoLiquidado                          : Result := '06';
      toRetornoLiquidadoEmCartorio                : Result := '08';
      toRetornoBaixaAutomatica                    : Result := '09';
      toRetornoBaixaPorTerSidoLiquidado           : Result := '10';
      toRetornoAbatimentoConcedido                : Result := '12';
      toRetornoAbatimentoCancelado                : Result := '13';
      toRetornoVencimentoAlterado                 : Result := '14';
      toRetornoBaixaRejeitada                     : Result := '15';
      toRetornoInstrucaoRejeitada                 : Result := '16';
      toRetornoRecebimentoInstrucaoProtestar      : Result := '19';
      toRetornoRecebimentoInstrucaoSustarProtesto : Result := '20';
      toRetornoAlteracaoSeuNumero                 : Result := '22';
      toRetornoEncaminhadoACartorio               : Result := '23';
      toRetornoRecebimentoInstrucaoNaoProtestar   : Result := '24';
      toRetornoTarifaMensalLiquidacoesBancosCorrespCarteira : Result := '28';
      toRetornoTarifaDeRelacaoDasLiquidacoes      : Result := '40';
      toRetornoBaixaPorProtesto                   : Result := '43';
      toRetornoTarifaInstrucao                    : Result := '96';
      toRetornoTarifaMensalBaixasCarteira         : Result := '97';
   end;
end;

function TACBrBancoPine.CodMotivoRejeicaoToDescricao(
  const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: Integer): String;
begin
   case TipoOcorrencia of
      toRetornoRegistroRecusado:
      case CodMotivo  of
         03: Result := '03-CEP inválido – Não temos cobrador – Cobrador não Localizado ';
         04: Result := '04-Sigla do Estado inválida ';
         05: Result := '05-Data de Vencimento inválida ou fora do prazo mínimo s';
         06: Result := '06-Código do Banco inválido ';
         08: Result := '08-Nome do sacado não informado ';
         10: Result := '10-Logradouro não informado';
         14: Result := '14-Registro em duplicidade';
         19: Result := '19-Data de desconto inválida ou maior que a data de vencimento';
         20: Result := '20-Valor de IOF não numérico';
         21: Result := '21-Movimento para título não cadastrado no sistema ';
         22: Result := '22-Valor de desconto + abatimento maior que o valor do título ';
         25: Result := '25-CNPJ ou CPF do sacado inválido (aceito com restrições) ';
         26: Result := '26-Espécie de documento inválida ';
         27: Result := '27-Data de emissão do título inválida';
         28: Result := '28-Seu número não informado';
         29: Result := '29-CEP é igual a espaço ou zeros; ou não numérico';
         30: Result := '30-Valor do título não numérico ou inválido ';
         36: Result := '36-Valor de permanência (mora) não numérico';
         37: Result := '37-Valor de permanência inconsistente, pois, dentro de um mês, será maior que o valor do título ';
         38: Result := '38-Valor de desconto/abatimento não numérico ou inválido';
         39: Result := '39-Valor de abatimento não numérico';
         42: Result := '42-Título já existente em nossos registros. Nosso número não aceito ';
         43: Result := '43-Título enviado em duplicidade nesse movimento ';
         44: Result := '44-Título zerado ou em branco; ou não numérico na remessa ';
         46: Result := '46-Título enviado fora da faixa de Nosso Número, estipulada para o cliente';
         51: Result := '51-Tipo/Número de Inscrição Sacador/Avalista Inválido ';
         52: Result := '52-Sacador/Avalista não informado';
         53: Result := '53-Prazo de vencimento do título excede ao da contratação';
         54: Result := '54-Banco informado não é nosso correspondente 140-142';
         55: Result := '55-Banco correspondente informado não cobra este CEP ou não possui faixas de CEP cadastradas';
         56: Result := '56-Nosso número no correspondente não foi informado';
         57: Result := '57-Remessa contendo duas instruções incompatíveis – não protestar e dias de protesto ou prazo para protesto inválido';
         58: Result := '58-Entradas Rejeitadas – Reprovado no Represamento para Análise';
         60: Result := '60-CNPJ/CPF do sacado inválido – título recusado';
         87: Result := '87-Excede Prazo máximo entre emissão e vencimento';
      else
        case AnsiIndexStr(IntToStr(CodMotivo),
                          ['AA', 'AB', 'AE', 'AI', 'AJ', 'AL', 'AU', 'AV', 'AX', 'BC',
                           'BD', 'BE', 'BF', 'BG', 'BH', 'CC', 'CD', 'CE', 'CF', 'CG',
                           'CH', 'CJ', 'CK', 'CS', 'DA', 'DB', 'DC', 'DD', 'DE', 'DG',
                           'DH', 'DI', 'DJ', 'DM', 'DN', 'DP', 'DT', 'EB', 'G1', 'G2',
                           'G3', 'G4', 'HA', 'HB', 'HC', 'HD', 'HF', 'HG', 'HH', 'HI',
                           'HJ', 'HK', 'HL', 'HM', 'HN', 'IX', 'JB', 'JC', 'JH', 'JI',
                           'JK', 'JK', 'JS', 'JT', 'KC', 'KD', 'KE', 'ZQ', 'ZR', 'ZS',
                           'ZT', 'ZU']) of
           00: Result := 'AA-Serviço de cobrança inválido';
           01: Result := 'AB-Serviço de "0" ou "5" e banco cobrador <> zero';
           02: Result := 'AE-Título não possui abatimento ';
           03: Result := 'AI-Nossa carteira inválida ';
           04: Result := 'AJ-Modalidade com bancos correspondentes inválida ';
           05: Result := 'AL-Sacado impedido de entrar nesta cobrança ';
           06: Result := 'AU-Data da ocorrência inválida ';
           07: Result := 'AV-Valor da tarifa de cobrança inválida ';
           08: Result := 'AX-Título em pagamento parcial';
           09: Result := 'BC-Análise gerencial-sacado inválido p/operação crédito ';
           10: Result := 'BD-Análise gerencial-sacado inadimplente ';
           11: Result := 'BE-Análise gerencial-sacado difere do exigido';
           12: Result := 'BF-Análise gerencial-vencto excede vencto da operação de crédito ';
           13: Result := 'BG-Análise gerencial-sacado com baixa liquidez ';
           14: Result := 'BH-Análise gerencial-sacado excede concentração ';
           15: Result := 'CC-Valor de iof incompatível com a espécie documento';
           16: Result := 'CD-Efetivação de protesto sem agenda válida';
           17: Result := 'CE-Título não aceito - pessoa física';
           18: Result := 'CF-Excede prazo máximo da entrada ao vencimento';
           19: Result := 'CG-Título não aceito – por análise gerencial';
           20: Result := 'CH-Título em espera – em análise pelo banco';
           21: Result := 'CJ-Análise gerencial-vencto do titulo abaixo przcurto';
           22: Result := 'CK-Análise gerencial-vencto do titulo abaixo przlongo ';
           23: Result := 'CS-Título rejeitado pela checagem de duplicatas';
           24: Result := 'DA-Análise gerencial – Entrada de Título Descontado com limite cancelado';
           25: Result := 'DB-Análise gerencial – Entrada de Título Descontado com limite vencido';
           26: Result := 'DC-Análise gerencial - cedente com limite cancelado';
           27: Result := 'DD-Análise gerencial – cedente é sacado e teve seu limite cancelado';
           28: Result := 'Análise gerencial - apontamento no Serasa';
           29: Result := 'DG-Endereço sacador/avalista não informado';
           30: Result := 'DH-Cep do sacador/avalista não informado ';
           31: Result := 'DI-Cidade do sacador/avalista não informado ';
           32: Result := 'DJ-Estado do sacador/avalista inválido ou n informado ';
           33: Result := 'DM-Cliente sem Código de Flash cadastrado no cobrador ';
           34: Result := 'DN-Título Descontado com Prazo ZERO – Recusado ';
           35: Result := 'DP-Data de Referência menor que a Data de Emissão do Título ';
           36: Result := 'DT-Nosso Número do Correspondente não deve ser informado ';
           37: Result := 'EB-HSBC não aceita endereço de sacado com mais de 38 caracteres ';
           38: Result := 'G1-Endereço do sacador incompleto ( lei 12.039)';
           39: Result := 'G2-Sacador impedido de movimentar';
           40: Result := 'G3-Concentração de cep não permitida';
           41: Result := 'G4-Valor do título não permitido';
           42: Result := 'HA-Serviço e Modalidade Incompatíveis';
           43: Result := 'HB-Inconsistências entre Registros Título e Sacador';
           44: Result := 'HC-Ocorrência não disponível';
           45: Result := 'HD-Título com Aceite';
           46: Result := 'HJ-Baixa Liquidez do Sacado ';
           47: Result := 'HG-Sacado Informou que não paga Boletos ';
           48: Result := 'HH-Sacado não confirmou a Nota Fiscal ';
           49: Result := 'HI-Checagem Prévia não Efetuada';
           50: Result := 'HJ-Sacado desconhece compra e Nota Fiscal ';
           51: Result := 'HK-Compra e Nota Fiscal canceladas pelo sacado';
           52: Result := 'HL-Concentração além do permitido pela área de Crédito ';
           53: Result := 'HM-Vencimento acima do permitido pelo área de Crédito';
           54: Result := 'HN-Excede o prazo limite da operação';
           55: Result := 'IX-Título de Cartão de Crédito não aceita instruções';
           56: Result := 'JB-Título de Cartão de Crédito inválido para o Produto';
           57: Result := 'JC-Produto somente para Cartão de Crédito';
           58: Result := 'JH-CB Direta com operação de Desconto Automático';
           59: Result := 'JI-Espécie de Documento incompatível para produto de Cartão de Crédito';
           60: Result := 'JK-Produto não permite alterar Valor e Vencimento';
           61: Result := 'JQ-Título em Correspondente – Alteração não permitida ';
           62: Result := 'JS-Título possui Desc/Abatim/Mora/Multa ';
           63: Result := 'JT-Título possui Agenda ';
           64: Result := 'KC-Título já Sustado';
           65: Result := 'KD-Serviço de Cobrança não permitido para carteira';
           66: Result := 'KE-Título possui caracteres não permitidos';
           67: Result := 'ZQ-Sem informação da Nota Fiscal Eletrônica ';
           68: Result := 'ZR-Chave de Acesso NF Rejeitada ';
           69: Result := 'ZS-Chave de Acesso NF Duplicada ';
           70: Result := 'ZT-Quantidade NF excede a quantidade permitida (30) ';
           71: Result := 'ZU-Chave de Acesso NF inválida';
        else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
        end;
      end;

      toRetornoBaixaRejeitada :
      case CodMotivo of
         05: Result:= '05-Solicitação de baixa para título já baixado ou liquidado';
         06: Result:= '06-Solicitação de baixa para título não registrado no sistema';
         08: Result:= '08-Solicitação de baixa para título em float';
      else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
      toRetornoInstrucaoRejeitada:
      case CodMotivo of
         04: Result := '04-Data de vencimento não numérica ou inválida';
         05: Result := '05-Data de Vencimento inválida ou fora do prazo mínimo s';
         14: Result := '14-Registro em duplicidade';
         19: Result := '19-Data de desconto inválida ou maior que a data de vencimento';
         20: Result := '20-Campo livre não informado';
         21: Result := '21-Título não registrado no sistema';
         22: Result := '22-Título baixado ou liquidado';
         26: Result := '26-Espécie de documento inválida ';
         27: Result := '27-Instrução não aceita, por não ter sido emitida ordem de protesto ao cartório';
         28: Result := '28-Título tem instrução de cartório ativa';
         29: Result := '29-Título não tem instrução de carteira ativa';
         36: Result := '36-Valor de permanência (mora) não numérico';
         37: Result := '37-Título Descontado – Instrução não permitida para a carteira';
         38: Result := '38-Valor do abatimento não numérico ou maior que a soma do valor do título + permanência + multa';
         39: Result := '39-Título em cartório';
         40: Result := '40-Instrução recusada – Reprovado no Represamento para Análise';
         44: Result := '44-Título zerado ou em branco; ou não numérico na remessa';
         51: Result := '51-Tipo/Número de Inscrição Sacador/Avalista Inválido';
         53: Result := '53-Prazo de vencimento do título excede ao da contratação';
         57: Result := '57-Remessa contendo duas instruções incompatíveis – não protestar e dias de protesto ou prazo para protesto inválido.';
         99: Result := '99-Ocorrência desconhecida na remessa.';
      else
         case AnsiIndexStr(IntToStr(CodMotivo),
                           ['AA', 'AB', 'AE', 'AI', 'AJ', 'AL', 'AU', 'AV', 'AX', 'BC',
                            'BD', 'BE', 'BF', 'BG', 'BH', 'CC', 'CD', 'CE', 'CF', 'CG',
                            'CH', 'CJ', 'CK', 'CS', 'DA', 'DB', 'DC', 'DD', 'DE', 'DG',
                            'DH', 'DI', 'DJ', 'DM', 'DN', 'DP', 'DT', 'EB', 'G1', 'G2',
                            'G3', 'G4', 'HA', 'HB', 'HC', 'HD', 'HF', 'HG', 'HH', 'HI',
                            'HJ', 'HK', 'HL', 'HM', 'HN', 'IX', 'JB', 'JC', 'JH', 'JI',
                            'JK', 'JK', 'JS', 'JT', 'KC', 'KD', 'KE', 'ZQ', 'ZR', 'ZS',
                            'ZT', 'ZU']) of
           00: Result := 'AA-Serviço de cobrança inválido';
           01: Result := 'AE-Título não possui abatimento ';
           02: Result := 'AG-Movimento não permitido – Título à vista ou contra apresentação';
           03: Result := 'AH-Cancelamento de valores inválidos';
           04: Result := 'AI-Nossa carteira inválida';
           05: Result := 'AK-Título pertence a outro cliente';
           06: Result := 'AU-Data da ocorrência inválida';
           07: Result := 'AY-Título deve estar em aberto e vencido para acatar protesto';
           08: Result := 'BA-Banco Correspondente Recebedor não é o Cobrador Atual';
           09: Result := 'BB-Título deve estar em cartório para baixar';
           10: Result := 'CB-Título possui protesto efetivado/a efetivar hoje';
           11: Result := 'CT-Título já baixado';
           12: Result := 'CW-Título já transferido';
           13: Result := 'DO-Título em Prejuízo';
           14: Result := 'IX-Título de Cartão de Crédito não aceita instruções';
           15: Result := 'JK-Produto não permite alteração de valor de título';
           16: Result := 'JQ-Título em Correspondente – Não alterar Valor';
           17: Result := 'JS-Título possui Descontos/Abto/Mora/Multa';
           18: Result := 'JT-Título possui Agenda de Protesto/Devolução';
         else
           Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
         end;
      end;
   else
      Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
   end;

   Result := ACBrSTr(Result);
end;

end.



