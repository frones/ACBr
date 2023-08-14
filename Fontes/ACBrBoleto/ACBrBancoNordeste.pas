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

unit ACBrBancoNordeste;

interface

uses
  Classes, SysUtils,
  ACBrBoleto, ACBrBoletoConversao;

type

  { TACBrBancoNordeste }

  TACBrBancoNordeste = class(TACBrBancoClass)
  private
    fSequencia: Integer;
  protected
  public
    property Sequencia : Integer read fSequencia  write fSequencia;
    Constructor create(AOwner: TACBrBanco);
    function CalcularDigitoVerificador(const ACBrTitulo:TACBrTitulo): String; override;
    function MontarCodigoBarras(const ACBrTitulo : TACBrTitulo): String; override;
    function MontarCampoNossoNumero(const ACBrTitulo :TACBrTitulo): String; override;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String; override;
    function MontarCampoCarteira(const ACBrTitulo: TACBrTitulo): String; override;
    procedure GerarRegistroHeader400(NumeroRemessa : Integer; aRemessa: TStringList); override;
    procedure GerarRegistroTransacao400(ACBrTitulo : TACBrTitulo; aRemessa: TStringList); override;
    procedure GerarRegistroTrailler400(ARemessa:TStringList);  override;
    Procedure LerRetorno400(ARetorno:TStringList); override;

    function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia) : String; override;
    function CodOcorrenciaToTipo(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
    function TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia):String; override;
    function CarteiraToTipoOperacao(const Carteira: string):String; 
    function CodMotivoRejeicaoToDescricao(const TipoOcorrencia:TACBrTipoOcorrencia; CodMotivo:Integer): String; override;
    function MotivoRejeicaoColuna(const Coluna: integer):string;

    function CodOcorrenciaToTipoRemessa(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
  end;

implementation

uses
  {$IFDEF COMPILER6_UP} dateutils {$ELSE} ACBrD5 {$ENDIF},
  StrUtils, ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings, ACBrUtil.DateTime;

{ TACBrBancoNordeste }

constructor TACBrBancoNordeste.create(AOwner: TACBrBanco);
begin
   inherited create(AOwner);
   fpDigito                := 3;
   fpNome                  := 'Banco do Nordeste';
   fpNumero                := 4;
   fpTamanhoMaximoNossoNum := 7;
   fpTamanhoAgencia        := 4;
   fpTamanhoConta          := 7;
   fpTamanhoCarteira       := 2; 
   fSequencia              := 1;
end;

function TACBrBancoNordeste.CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo ): String;
begin
   Modulo.CalculoPadrao;
   Modulo.MultiplicadorFinal := 8;
   Modulo.Documento := ACBrTitulo.NossoNumero;
   Modulo.Calcular;

   Result:= IntToStr(Modulo.DigitoFinal);
end;

function TACBrBancoNordeste.MontarCodigoBarras ( const ACBrTitulo: TACBrTitulo) : String;
var
  CodigoBarras, FatorVencimento, DigitoCodBarras:String;
begin
   with ACBrTitulo.ACBrBoleto do
   begin
      FatorVencimento := CalcularFatorVencimento(ACBrTitulo.Vencimento);

      CodigoBarras := IntToStrZero( Numero, 3 )+'9'+ FatorVencimento +
                      IntToStrZero(Round(ACBrTitulo.ValorDocumento*100),10) +
                      PadLeft(OnlyNumber(Cedente.Agencia),4,'0') +
                      PadLeft(OnlyNumber(Cedente.Conta),7,'0') +
                      PadLeft(Cedente.ContaDigito,1,'0') +
                      ACBrTitulo.NossoNumero +
                      CalcularDigitoVerificador(ACBrTitulo) +
                      CarteiraToTipoOperacao(ACBrTitulo.Carteira) + '000';

      DigitoCodBarras := CalcularDigitoCodigoBarras(CodigoBarras);
   end;

   Result:= IntToStrZero(Numero, 3) + '9'+ DigitoCodBarras + Copy(CodigoBarras,5,39);
end;

function TACBrBancoNordeste.MontarCampoNossoNumero (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
   Result:= ACBrTitulo.NossoNumero+'-'+CalcularDigitoVerificador(ACBrTitulo);
end;

function TACBrBancoNordeste.MontarCampoCodigoCedente (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
   Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia+' / '+
             ACBrTitulo.ACBrBoleto.Cedente.Conta+'-'+
             ACBrTitulo.ACBrBoleto.Cedente.ContaDigito;
end;

function TACBrBancoNordeste.MontarCampoCarteira(const ACBrTitulo: TACBrTitulo
  ): String;
begin
  Result:= CarteiraToTipoOperacao(ACBrTitulo.Carteira);
end;

procedure TACBrBancoNordeste.GerarRegistroHeader400(NumeroRemessa : Integer; aRemessa:TStringList);
var
  wLinha: String;
begin
   with ACBrBanco.ACBrBoleto.Cedente do
   begin
      wLinha:= '0'                                        + // ID do Registro
               '1'                                        + // ID do Arquivo( 1 - Remessa)
               'REMESSA'                                  + // Literal de Remessa
               '01'                                       + // Código do Tipo de Serviço
               PadRight( 'COBRANCA', 15 )                 + // Descrição do tipo de serviço
               PadLeft(OnlyNumber(Agencia), 4, '0')       + // Cód. da Agência do cliente
               IntToStrZero(0, 2)                         + // Filler - Zeros
               PadLeft(OnlyNumber(Conta), 7, '0')         + // Conta corrente de cobrança
               PadLeft( ContaDigito, 1, '0')              + // Dígito da conta corrente
               Space(6)                                   + // Filler - Brancos
               PadRight( Nome, 30)                        + // Nome da Empresa
               IntToStrZero( Numero, 3 )+
               PadRight('B. DO NORDESTE', 15)             + // Código e Nome do Banco(400 - B. DO NORDESTE)
               FormatDateTime('ddmmyy',Now)               + // Data de geração do arquivo
               Space(294)                                 + // Brancos
               IntToStrZero(1,6);                           // Contador

      aRemessa.Text:= aRemessa.Text + UpperCase(wLinha);
   end;
end;

procedure TACBrBancoNordeste.GerarRegistroTransacao400(ACBrTitulo :TACBrTitulo; aRemessa: TStringList);
var
  DigitoNossoNumero, Ocorrencia, aEspecie, aAgencia :String;
  Protesto, TipoSacado, MensagemCedente, aConta     :String;
  wLinha, wAceite, wDiasProtesto: String;
  WCarteira: Char;
  aPercMulta: Double;
begin

   with ACBrTitulo do
   begin
      DigitoNossoNumero := CalcularDigitoVerificador(ACBrTitulo);

      aAgencia := IntToStrZero(StrToIntDef(OnlyNumber(ACBrBoleto.Cedente.Agencia),0),4);
      aConta   := IntToStrZero(StrToIntDef(OnlyNumber(ACBrBoleto.Cedente.Conta),0),7) + IntToStrZero(StrToIntDef(trim(ACBrBoleto.Cedente.ContaDigito),0),1);

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

      {Pegando campo Intruções}

      {if (DataProtesto > 0) and (DataProtesto > Vencimento) then
          Protesto := '06' + IntToStrZero(DaysBetween(DataProtesto,Vencimento),2)
      else if Ocorrencia = '31' then
         Protesto := '9999'
      else
         Protesto := PadLeft(trim(Instrucao1),2,'0') + PadLeft(trim(Instrucao2),2,'0');}

      //TK-2106
      Protesto := '0000';
      if naoEstaVazio(Trim(Instrucao1)) or naoEstaVazio(trim(Instrucao2)) then
        Protesto := PadLeft(trim(Instrucao1),2,'0') + PadLeft(trim(Instrucao2),2,'0');


      {Pegando Tipo de Sacado}
      case Sacado.Pessoa of
         pFisica   : TipoSacado := '01';
         pJuridica : TipoSacado := '02';
      else
         TipoSacado := '99';
      end;

      if ACBrBoleto.Cedente.CaracTitulo = tcSimples then
      begin
        if ACBrBoleto.Cedente.ResponEmissao = tbBancoEmite then
          wCarteira:= '1'
        else
          wCarteira:= '4';
      end
      else if ACBrBoleto.Cedente.CaracTitulo = tcVinculada then
      begin
        if ACBrBoleto.Cedente.ResponEmissao = tbBancoEmite then
          wCarteira:= '2'
        else
          wCarteira:= '5';
      end
      else
        WCarteira:= 'I';

      if Aceite = atSim then
        wAceite := 'S'
      else
        wAceite := 'N';

      if DiasDeProtesto > 0 then
        wDiasProtesto := FormatFloat('00', DiasDeProtesto)
      else
        wDiasProtesto := '99';

      if MultaValorFixo then
        if ValorDocumento > 0 then
          aPercMulta := (PercentualMulta / ValorDocumento) * 100
        else
          aPercMulta := 0
      else
        aPercMulta := PercentualMulta;

      with ACBrBoleto do
      begin
         if Mensagem.Text<>'' then
         MensagemCedente:= Mensagem[0];

         wLinha:= '1'                                                     +  // ID Registro
                  Space(16)                                               +  // Filler - Brancos
                  PadLeft( aAgencia, 4, '0')                              +  // Cód. da Agência do cliente
                  IntToStrZero(0, 2)                                      +  // Filler - Zeros
                  PadLeft( aConta, 7, '0')                                +  // Conta Corrente de Cobrança + Dígito da Conta Corrente
                  PadLeft( Cedente.ContaDigito, 1, '0')                   +  // Dígito da conta corrente
                  IntToStrZero( Trunc( aPercMulta), 2)                    +  // Percentual de Multa por atraso
                  Space(4)                                                +  // Filler - Brancos
                  PadRight( SeuNumero,25,' ')                             +  // Numero de Controle do Participante
                  NossoNumero + DigitoNossoNumero                         +
                  PadLeft( '0', 10, '0')                                  +  //Número do Contrato para cobrança caucionada/vinculada. Preencher com zeros para cobrança simples
                  PadLeft( '0', 6, '0')                                   +  //Número do Contrato para cobrança caucionada/vinculada. Preencher com zeros para cobrança simples
                  IntToStrZero(round( ValorDesconto * 100), 13)           +
                  Space(8)                                                +  // Filler - Brancos
                  wCarteira                                               +  // Carteira a ser utilizada
                  Ocorrencia                                              +  // Ocorrência
                  PadRight( NumeroDocumento,  10)                         +
                  FormatDateTime( 'ddmmyy', Vencimento)                   +
                  IntToStrZero( Round( ValorDocumento * 100 ), 13)        +
                  StringOfChar('0', 7) + Space(1) + PadRight(aEspecie, 2) + wAceite +  // Zeros + Filler + Especie do documento + Idntificação(valor fixo N)
                  FormatDateTime( 'ddmmyy', DataDocumento )               +  // Data de Emissão
                  Protesto                                                +
                  IntToStrZero( round(ValorMoraJuros * 100 ), 13)         +
                  IfThen(DataDesconto < EncodeDate(2000,01,01),'000000',
                         FormatDateTime( 'ddmmyy', DataDesconto))         +
                  IntToStrZero( round( ValorDesconto * 100 ), 13)         +
                  IntToStrZero( round( ValorIOF * 100 ), 13)              +
                  IntToStrZero( round( ValorAbatimento * 100 ), 13)       +
                  TipoSacado + PadLeft(OnlyNumber(Sacado.CNPJCPF),14,'0') +
                  PadRight( Sacado.NomeSacado, 40, ' ')                       +
                  PadRight( Sacado.Logradouro + ' ' + Sacado.Numero, 40, ' ') +
                  PadRight( Sacado.Complemento, 12, ' ')                      +
                  PadRight( Sacado.CEP, 8 )                                   +
                  PadRight( Sacado.Cidade, 15 )                               +
                  PadRight( Sacado.UF, 2 )                                    +
                  PadRight( MensagemCedente, 40 )                             +
                  wDiasProtesto + '0'                   +

                  IntToStrZero(aRemessa.Count + 1, 6); // Nº SEQÜENCIAL DO REGISTRO NO ARQUIVO

         aRemessa.Text:= aRemessa.Text + UpperCase(wLinha);
      end;
   end;
end;

procedure TACBrBancoNordeste.GerarRegistroTrailler400( ARemessa:TStringList );
var
  wLinha: String;
begin
   wLinha:= '9' + Space(393)                     + // ID Registro
            IntToStrZero( ARemessa.Count + 1, 6);       // Contador de Registros

   ARemessa.Text:= ARemessa.Text + UpperCase(wLinha);
end;

procedure TACBrBancoNordeste.LerRetorno400(ARetorno: TStringList);
var
  Titulo : TACBrTitulo;
  ContLinha, i: integer;
  rAgencia, rConta, rDigitoConta, Linha, rCedente, rCNPJCPF: String;
begin


   if StrToIntDef(copy(ARetorno.Strings[0],77,3),-1) <> Numero then
      raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                             'não é um arquivo de retorno do '+ Nome));

   rCedente := trim(Copy(ARetorno[0],47,30));

   rAgencia := trim(Copy(ARetorno[0], 27, ACBrBanco.TamanhoAgencia));   // alterado de linha 1 para 0 e posição 26 p/ 27 p/ LP Sistemas em 01/12/2015
   rConta   := trim(Copy(ARetorno[0], 33, ACBrBanco.TamanhoConta));     // alterado de linha 1 para 0 e posição 31 p/ 33 p/ LP Sistemas em 01/12/2015

   rDigitoConta := Copy(ARetorno[0],40,1);                              // alterado de linha 1 para 0 e posição 37 p/ 40 p/ LP Sistemas em 01/12/2015

   ACBrBanco.ACBrBoleto.NumeroArquivo := StrToIntDef(Copy(ARetorno[0],109,5),0);

   if Copy(ARetorno[0],95,2) <> '00' then
     ACBrBanco.ACBrBoleto.DataArquivo:= StringToDateTimeDef( Copy(ARetorno[0],95,2)+'/'+
                                                             Copy(ARetorno[0],97,2)+'/'+
                                                             Copy(ARetorno[0],99,2),0, 'DD/MM/YY' );
   if Copy(ARetorno[0],120,2) <> '00' then
     ACBrBanco.ACBrBoleto.DataCreditoLanc := StringToDateTimeDef( Copy(ARetorno[0],120,2)+'/'+
                                                                  Copy(ARetorno[0],122,2)+'/'+
                                                                  Copy(ARetorno[0],124,2),0, 'DD/MM/YY' );

   case StrToIntDef(Copy(ARetorno[1],2,2),0) of
      11: rCNPJCPF := Copy(ARetorno[1],7,11);
      14: rCNPJCPF := Copy(ARetorno[1],4,14);
   else
     rCNPJCPF := Copy(ARetorno[1],4,14);
   end;

   ValidarDadosRetorno(rAgencia, rConta, rCNPJCPF);
   with ACBrBanco.ACBrBoleto do
   begin
      Cedente.Nome    := rCedente;
      Cedente.CNPJCPF := rCNPJCPF;
      Cedente.Agencia := rAgencia;
      Cedente.AgenciaDigito:= '0';
      Cedente.Conta   := rConta;
      Cedente.ContaDigito:= rDigitoConta;

      case StrToIntDef(Copy(ARetorno[1],2,2),0) of
         11,01: Cedente.TipoInscricao:= pFisica;
         14,02: Cedente.TipoInscricao:= pJuridica;
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

         for i := 0 to 76 do
         begin
           if (copy(Linha,280+i,1)='1') then
           begin
             if ((i+280 < 296) or
                 (i+280 > 301) or 
                 (OcorrenciaOriginal.Tipo <> toRetornoLiquidado)) then
             DescricaoMotivoRejeicaoComando.Add(MotivoRejeicaoColuna(280+i));
           end;
         end;

         
         if Copy(Linha,111,2) <> '00' then
           DataOcorrencia := StringToDateTimeDef( Copy(Linha,111,2)+'/'+
                                                  Copy(Linha,113,2)+'/'+
                                                  Copy(Linha,115,2),0, 'DD/MM/YY' );
         if Copy(Linha,147,2 )<>'00' then
            Vencimento := StringToDateTimeDef( Copy(Linha,147,2)+'/'+
                                               Copy(Linha,149,2)+'/'+
                                               Copy(Linha,151,2),0, 'DD/MM/YY' );

         ValorDocumento       := StrToFloatDef(Copy(Linha,153,13),0)/100;
         ValorDespesaCobranca := StrToFloatDef(Copy(Linha,176,13),0)/100;
         ValorOutrasDespesas  := StrToFloatDef(Copy(Linha,189,13),0)/100;
         ValorIOF             := StrToFloatDef(Copy(Linha,215,13),0)/100;
         ValorAbatimento      := StrToFloatDef(Copy(Linha,228,13),0)/100;
         ValorDesconto        := StrToFloatDef(Copy(Linha,241,13),0)/100;
         ValorRecebido        := StrToFloatDef(Copy(Linha,254,13),0)/100;
         ValorMoraJuros       := StrToFloatDef(Copy(Linha,267,13),0)/100;
         NossoNumero          := Copy(Linha,63,7);
         Carteira             := Copy(Linha,108,1);
         

         if (OcorrenciaOriginal.Tipo = toRetornoLiquidado) and 
            (StrToIntDef(Copy(Linha,296,6),0) <> 0) then
            DataCredito:= StringToDateTimeDef( Copy(Linha,296,2)+'/'+
                                               Copy(Linha,298,2)+'/'+
                                               Copy(Linha,300,2),0, 'DD/MM/YY' );
      end;
   end;
end;

function TACBrBancoNordeste.TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String;
var
 CodOcorrencia: Integer;
begin

  CodOcorrencia := StrToIntDef(TipoOcorrenciaToCod(TipoOcorrencia),0);

  case CodOcorrencia of
    02: Result := '02-Entrada Confirmada';
    04: Result := '04-Alteraçao';
    06: Result := '06-Liquidação normal';
    07: Result := '07-Pagamento por Conta';
    08: Result := '08-Liquidação em Cartório' ;
    09: Result := '09-Baixa Simples';
    10: Result := '10-Devolvido/Protestado';
    11: Result := '11-Titulo em Ser' ;
    12: Result := '12-Abatimento Concedido' ;
    13: Result := '13-Abatimento Cancelado' ;
    14: Result := '14-Vencimento Alterado' ;
    15: Result := '15-Baixa Automática';
    18: Result := '18-Acerto de Depositária' ;
    19: Result := '19-Confirmação de Protesto';
    20: Result := '20-Confirmação de Sustar Protesto';
    21: Result := '21-Acerto do Controle do Participante';
    22: Result := '22-Alteração de Seu Número';
    51: Result := '51-Entrada Rejeitada' ;
  end;

  Result := ACBrSTr(Result);
end;

function TACBrBancoNordeste.CodOcorrenciaToTipo(const CodOcorrencia:
   Integer ) : TACBrTipoOcorrencia;
begin
   case CodOcorrencia of
      02: Result := toRetornoRegistroConfirmado;
      04: Result := toRetornoAlteracaoUsoCedente; // alteração
      06: Result := toRetornoLiquidado;
      07: Result := toRetornoLiquidadoPorConta;
      08: Result := toRetornoLiquidadoEmCartorio;
      09: Result := toRetornoBaixaSimples;
      10: Result := toRetornoBaixaPorProtesto;    // Devolvido/Protestado
      11: Result := toRetornoTituloEmSer;
      12: Result := toRetornoAbatimentoConcedido;
      13: Result := toRetornoAbatimentoCancelado;
      14: Result := toRetornoVencimentoAlterado;
      15: Result := toRetornoBaixaAutomatica;
      18: Result := toRetornoAcertoDepositaria;
      19: Result := toRetornoRecebimentoInstrucaoProtestar;
      20: Result := toRetornoRecebimentoInstrucaoSustarProtesto;
      21: Result := toRetornoAcertoControleParticipante;
      22: Result := toRetornoAlteracaoSeuNumero;
      51: Result := toRetornoRegistroRecusado;
   else
      Result := toRetornoOutrasOcorrencias;
   end;
end;

function TACBrBancoNordeste.CodOcorrenciaToTipoRemessa(const CodOcorrencia: Integer): TACBrTipoOcorrencia;
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

function TACBrBancoNordeste.TipoOcorrenciaToCod (
   const TipoOcorrencia: TACBrTipoOcorrencia ) : String;
begin
   case TipoOcorrencia of
      toRetornoRegistroConfirmado   				: Result := '02';
      toRetornoAlteracaoUsoCedente  				: Result := '04';   // alteração
      toRetornoLiquidado            				: Result := '06';
      toRetornoLiquidadoPorConta    				: Result := '07';
      toRetornoLiquidadoEmCartorio  				: Result := '08';
      toRetornoBaixaSimples         				: Result := '09';
      toRetornoBaixaPorProtesto     				: Result := '10';   // Devolvido/Protestado
      toRetornoTituloEmSer          				: Result := '11';
      toRetornoAbatimentoConcedido  				: Result := '12';
      toRetornoAbatimentoCancelado  				: Result := '13';
      toRetornoVencimentoAlterado   				: Result := '14';
      toRetornoBaixaAutomatica      				: Result := '15';
      toRetornoAcertoDepositaria    				: Result := '18';
      toRetornoRecebimentoInstrucaoProtestar      	: Result := '19';
      toRetornoRecebimentoInstrucaoSustarProtesto 	: Result := '20';
      toRetornoAcertoControleParticipante         	: Result := '21';
      toRetornoAlteracaoSeuNumero                 	: Result := '22';
      toRetornoRegistroRecusado                   	: Result := '51';
   else
      Result:= '02';
   end;
end;

function TACBrBancoNordeste.CarteiraToTipoOperacao(const Carteira: string):String; 
begin
  if Carteira = '01' then
    Result:= '21'
  else if Carteira = '02' then
    Result:= '41'
  else if Carteira = '04' then
    Result:= '21'
  else if Carteira = '05' then
    Result:= '41'
  else if Carteira = '06' then
    Result:= '31'
  else if Carteira = 'I' then
    Result:= '51'
  else
    Result:= Carteira;
end;


function TACBrBancoNordeste.CodMotivoRejeicaoToDescricao(
  const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: Integer): String;
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

function TACBrBancoNordeste.MotivoRejeicaoColuna(const Coluna: integer):string; //override;
begin
  case Coluna of
    280: result:= '01-Falta valor do IOC.';
    281: result:= '02-Não permite desconto/ abatimento.';
    282: result:= '03-Código do serviço inválido' ;
    283: result:= '04-Novo vencimento igual/ menor que o da entrada.';
    284: result:= '05-Novo vencimento igual ao do Título.';
    285: result:= '06-Espécie Documento Inválida.';
    286: result:= '07-Espécie Documento Inexistente.';
    287: result:= '08-Tipo Operação Inválida.';
    288: result:= '09-Tipo Operação Inexistente.';
    289: result:= '10-Contrato Proibido para esta Carteira.';
    290: result:= '11-Falta Número do Contrato.';
    291: result:= '12-Proibido Informar Tipo de Conta.';
    292: result:= '13-Tipo de Conta do Contrato Inexistente.';
    293: result:= '14-Dígito de Contrato não confere.';
    294: result:= '15-Contrato Inexistente.';
    295: result:= '16-Data de Emissão Inválida.';
    296: result:= '17-Falta Valor do Título.';
    297: result:= '18-Vencimento Inválido.';
    298: result:= '19-Data Vencimento Anterior a Emissão.';
    299: result:= '20-Falta Vencimento Desconto.';
    300: result:= '21-Data Desconto Inválida.';
    301: result:= '22-Data Desconto Posterior ao Vencimento.';
    302: result:= '23-Falta Valor Desconto.';
    303: result:= '24-Falta Mora-1-Dia.';
    304: result:= '25-Banco/Agência Cobrador Inexistente.';
    305: result:= '26-BCO/AGE Cobrador não Cadastrado.';
    306: result:= '27-Código Pessoa Inválido.';
    307: result:= '28-Falta CEP, Banco e Agência Cobrador.';
    308: result:= '29-Falta Nome Sacado.';
    309: result:= '30-Falta Endereço.';
    310: result:= '31-Falta Cidade.';
    311: result:= '32-Falta Estado.';
    312: result:= '33-Estado Inválido.';
    313: result:= '34-Falta CPF/ CGC do Sacado.';
    314: result:= '35-Falta numeração - Bloquete emitido.';
    315: result:= '36-Título Pré-Numerado já Existente.';
    316: result:= '37-Dígito do Título Não Confere.';
    317: result:= '38-Proibido Protestar.';
    318: result:= '39-Proibido título pré-numerado p/ Correspondente.';
    319: result:= '40-Dígito Cliente/ Contrato com Erro.';
    320: result:= '41-Dígito Nosso Número com Erro.';
    321: result:= '42-Título Inexistente.';
    322: result:= '43-Título Liquidado.';
    323: result:= '44-Título Não Pode Ser Baixado.';
    324: result:= '45-Valor Nominal Incorreto.';
    325: result:= '46-Proibido Taxa – Multa p/ Correspondente.';
    326: result:= '47-Falta Tipo de Conta do Contrato.';
    327: result:= '48-Tipo de Conta Inexistente.';
    328: result:= '49-Dígito Contrato Não Confere.';
    329: result:= '50-Dígito do Título Não Confere.';
    330: result:= '51-Título Inexistente ou Liquidado.';
    331: result:= '52-Valor Abatimento Inválido.';
    332: result:= '53-Data Vencimento Inválida.';
    333: result:= '54-Estado Inválido.';
    334: result:= '55-Falta Tipo de Pessoa P/ Alteração de CGC/ CPF.';
    335: result:= '56-CPF/ CGC com Erro.';
    336: result:= '57-Data Emissão Inválida.';
    337: result:= '58-Data Vencimento Desconto Inválida.';
    338: result:= '59-Aceite Inválido para Espécie Documento.';
    339: result:= '60-Não Aceite Inválido para Espécie Documento.';
    340: result:= '61-Banco/ Agência Cobrador Inválido.';
    341: result:= '62-Limite Operacional Não Cadastrado.';
    342: result:= '63-Título já em situação de protesto.';
    343: result:= '64-Proibido alterar vencimento título descontado.';
    344: result:= '65-Proibido informar nosso número p/ cod. carteira.';
    345: result:= '66-Falta vencimento desconto-2.';
    346: result:= '67-Data desconto-2 inválida.';
    347: result:= '68-Data desconto-2 posterior ao vencimento.';
    348: result:= '69-Falta valor desconto-2.';
    349: result:= '70-Data vencimento desconto-2 inválida.';
    350: result:= '71-IOC maior que valor do título.';
    351: result:= '72-CEP não pertence ao Estado.';
    352: result:= '73-Seu número já existente.';
    353: result:= '74-Moeda Inválida para o tipo de Operação.';
    354: result:= '75-Moeda inexistente.';
    355: result:= '76-Nosso número/ dígito com erro.';
    356: result:= '77-Dias vencidos superior ao prazo de devolução.';
  end;

  Result := ACBrSTr(Result);
end;

end.


