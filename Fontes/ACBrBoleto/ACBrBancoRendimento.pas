{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:   Denis Cesar Zago, Victor H Gonzales - Pandaaa }
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

unit ACBrBancoRendimento;

interface

uses
  Classes, Contnrs, SysUtils, ACBrBoleto;

type

  { TACBrBancoRendimento }

  TACBrBancoRendimento = class(TACBrBancoClass)
  private
  protected
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

    function CodOcorrenciaToTipoRemessa(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
  end;

implementation

uses {$IFDEF COMPILER6_UP} dateutils {$ELSE} ACBrD5 {$ENDIF},
  StrUtils, ACBrUtil.Base, ACBrBoletoConversao, ACBrUtil.Strings, ACBrUtil.DateTime ;

{ TACBrBancoRendimento }

constructor TACBrBancoRendimento.create(AOwner: TACBrBanco);
begin
   inherited create(AOwner);
   fpDigito                 := 2;
   fpNome                   := 'RENDIMENTO';
   fpNumero                 := 633;
   fpTamanhoMaximoNossoNum  := 11;   
   fpTamanhoAgencia         := 4;
   fpTamanhoConta           := 7;
   fpTamanhoCarteira        := 2;
end;

function TACBrBancoRendimento.CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo ): String;
begin
   Modulo.CalculoPadrao;
   Modulo.MultiplicadorFinal := 7;
   Modulo.Documento := ACBrTitulo.Carteira + ACBrTitulo.NossoNumero;
   Modulo.Calcular;

   if Modulo.ModuloFinal = 1 then
      Result:= 'P'
   else
      Result:= IntToStr(Modulo.DigitoFinal);
end;

function TACBrBancoRendimento.MontarCodigoBarras ( const ACBrTitulo: TACBrTitulo) : String;
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
                      ACBrTitulo.NossoNumero +
                      PadLeft(RightStr(Cedente.Conta,7),7,'0') + '0';

      DigitoCodBarras := CalcularDigitoCodigoBarras(CodigoBarras);
   end;

   Result:= IntToStr(Numero) + '9'+ DigitoCodBarras + Copy(CodigoBarras,5,39);
end;

function TACBrBancoRendimento.MontarCampoNossoNumero (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
   Result:= ACBrTitulo.Carteira+'/'+ACBrTitulo.NossoNumero+'-'+CalcularDigitoVerificador(ACBrTitulo);
end;

function TACBrBancoRendimento.MontarCampoCodigoCedente (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
   Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia+'-'+
             ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito+'/'+
             ACBrTitulo.ACBrBoleto.Cedente.Conta+'-'+
             ACBrTitulo.ACBrBoleto.Cedente.ContaDigito;
end;


procedure TACBrBancoRendimento.GerarRegistroHeader400(NumeroRemessa : Integer; ARemessa:TStringList);
var
  wLinha: String;
begin
   with ACBrBanco.ACBrBoleto.Cedente do
   begin
      wLinha:= '0'                                             + // Código de Registro
               '1'                                             + // Código de Remessa ( 1 - Remessa)
               'REMESSA'                                       + // Literal de Remessa
               '01'                                            + // Código do Serviço
               PadRight( 'COBRANCA', 15 )                      + // Literal d Serviço
               PadLeft( CodigoCedente, 14, '0')+Space(6)       + // Código da Empresa+Espaço Em Brancos
               PadRight( Nome, 30)                             + // Nome da Empresa
               IntToStr( Numero )+ PadRight(fpNome, 15)        + // Código e Nome do Banco(633 - Banco Rendimento)
               FormatDateTime('ddmmyy',Now)  + Space(294)      + // Data de geração do arquivo + brancos
               IntToStrZero(1,6);                                // Nr. Sequencial de Remessa + brancos + Contador

      ARemessa.Text:= ARemessa.Text + UpperCase(wLinha);
   end;
end;

procedure TACBrBancoRendimento.GerarRegistroTransacao400(ACBrTitulo :TACBrTitulo; aRemessa: TStringList);
var
  DigitoNossoNumero, Ocorrencia, aEspecie, aAgencia :String;
  Protesto, TipoSacado, MensagemCedente, aConta, CartTitulo:String;
  aCarteira, wLinha, ANossoNumero: String;
  aPercMulta: Double;

  function DoMontaInstrucoes1: string;
  begin
     Result := '';
     with ACBrTitulo, ACBrBoleto do
     begin

        {Primeira instrução vai no registro 1}
        if Mensagem.Count <= 1 then
        begin
           Result := '';
           Exit;
        end;

        Result := sLineBreak + '20'+ Copy(PadRight(Mensagem[1], 80, ' '), 1, 80); // IDENTIFICAÇÃO DO LAYOUT PARA O REGISTRO // CONTEÚDO DA 1ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO

        if Mensagem.Count >= 3 then
           Result := Result + Copy(PadRight(Mensagem[2], 80, ' '), 1, 80)     // CONTEÚDO DA 2ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO
        else
           Result := Result + PadRight('', 80, ' ');                          // CONTEÚDO DO RESTANTE DAS LINHAS

        if Mensagem.Count >= 4 then
           Result := Result + Copy(PadRight(Mensagem[3], 80, ' '), 1, 80)     // CONTEÚDO DA 3ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO
        else
           Result := Result + PadRight('', 80, ' ');                          // CONTEÚDO DO RESTANTE DAS LINHAS

        if Mensagem.Count >= 5 then
           Result := Result + Copy(PadRight(Mensagem[4], 80, ' '), 1, 80)     // CONTEÚDO DA 4ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO
        else
           Result := Result + PadRight('', 80, ' ');                          // CONTEÚDO DO RESTANTE DAS LINHAS

        Result := Result                                              +       // 001 a 321 - Mensagens
                  space(72)                                           +       // 322 a 394 - BRANCOS
                  IntToStrZero( aRemessa.Count + 2, 6);                       // 395 a 400 - Nº SEQÜENCIAL DO REGISTRO NO ARQUIVO

     end;
  end;

begin

   with ACBrTitulo do
   begin
      ANossoNumero := PadLeft(OnlyNumber(ACBrTitulo.NossoNumero),11, '0');

      if (ACBrBoleto.Cedente.ResponEmissao = tbBancoEmite) and (StrToInt64Def(ANossoNumero,0) = 0) then
        DigitoNossoNumero := '0'
      else
      begin
        ANossoNumero      := ACBrTitulo.NossoNumero;
        DigitoNossoNumero := CalcularDigitoVerificador(ACBrTitulo);
      end;


      aAgencia := IntToStrZero(StrToIntDef(OnlyNumber(ACBrBoleto.Cedente.Agencia),0),5);
      aConta   := IntToStrZero(StrToIntDef(OnlyNumber(ACBrBoleto.Cedente.Conta),0),7);
      aCarteira:= IntToStrZero(StrToIntDef(trim(Carteira),0), 3);

      {Pegando Código da Ocorrencia}
      case OcorrenciaOriginal.Tipo of
         toRemessaBaixar                         : Ocorrencia := '02'; {Pedido de Baixa}
         toRemessaProtestoFinsFalimentares       : Ocorrencia := '03'; {Pedido de Protesto Falimentar}
         toRemessaConcederAbatimento             : Ocorrencia := '04'; {Concessão de Abatimento}
         toRemessaCancelarAbatimento             : Ocorrencia := '05'; {Cancelamento de Abatimento concedido}
         toRemessaAlterarVencimento              : Ocorrencia := '06'; {Alteração de vencimento}
         toRemessaAlterarControleParticipante    : Ocorrencia := '07'; {Alteração do controle do participante}
         toRemessaAlterarNumeroControle          : Ocorrencia := '08'; {Alteração de seu número}
         toRemessaProtestar                      : Ocorrencia := '09'; {Pedido de protesto}
         toRemessaCancelarInstrucaoProtestoBaixa : Ocorrencia := '18'; {Sustar protesto e baixar}
         toRemessaCancelarInstrucaoProtesto      : Ocorrencia := '19'; {Sustar protesto e manter na carteira}
         toRemessaAlterarValorTitulo             : Ocorrencia := '20'; {Alteração de valor}
         toRemessaTransferenciaCarteira          : Ocorrencia := '23'; {Transferência entre carteiras}
         toRemessaDevTransferenciaCarteira       : Ocorrencia := '24'; {Dev. Transferência entre carteiras}
         toRemessaOutrasOcorrencias              : Ocorrencia := '31'; {Alteração de Outros Dados}
      else
         Ocorrencia := '01';                                           {Remessa}
      end;

      if NossoNumero = EmptyStr then
        DigitoNossoNumero := '0';

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
      else if trim(EspecieDoc) = 'OU' then
         aEspecie:= '99'
      else
         aEspecie := EspecieDoc;

      if (DataProtesto > 0) and (DataProtesto > Vencimento) then
        Protesto := IntToStrZero(DaysBetween(DataProtesto,Vencimento), 2)
      else
        Protesto := '00';

      {Pegando Tipo de Sacado}
      case Sacado.Pessoa of
         pFisica   : TipoSacado := '01';
         pJuridica : TipoSacado := '02';
      else
         TipoSacado := '04'; //04 –CNPJ do sacador
      end;

      { Converte valor em moeda para percentual, pois o arquivo só permite % }
      if MultaValorFixo then
        if ValorDocumento > 0 then
          aPercMulta := (PercentualMulta / ValorDocumento) * 100
        else
          aPercMulta := 0
      else
        aPercMulta := PercentualMulta;

      case CaracTitulo of
        tcSimples:    CartTitulo := '1';
        tcVinculada:  CartTitulo := '2';
      end;

      with ACBrBoleto do
      begin
         if Mensagem.Text <> '' then
            MensagemCedente:= Mensagem[0];

                  wLinha:= '1'                                            +  // 001 a 001 - ID Registro
                  TipoSacado                                              +  // 002 a 003 Tipo de inscrição da empresa
                  PadLeft(OnlyNumber(Cedente.CNPJCPF), 14, '0')           +  // 004 a 017 Número de inscrição
                  PadLeft(Cedente.CodigoCedente, 14, '0')                 +  // 018 a 031 Código da empresa no banco
                  Space(31)                                               +  // 032 a 062 Uso exclusivo da empresa
                  StringOfChar('0',11)                                    +  // 063 a 073 - Zeros
                  Space(16)                                               +  // 074 a 089
                  IfThen( PercentualMulta > 0, '2', '0')                  +  // 090 a 090 - Indica se exite Multa ou não
                  IntToStrZero( round( aPercMulta * 10000 ) , 13)         +  // 091 a 103 - Percentual de Multa formatado com 4 casas decimais
                  ('01')                                                  +  // 104 a 105 - Número de dias para Multa.
                  Space(2)                                                +  // 106 a 107 - Brancos
                  CartTitulo                                              +  // 108 a 108 - Código da carteira - Tipo : 1 - COBRANÇA SIMPLES , 2 - COBRANÇA VINCULADA
                  Ocorrencia                                              +  // 109 a 110 - Código da ocorrência
                  PadRight( NumeroDocumento,  9)+' '                      +  // 111 a 120 - Numero Documento
                  FormatDateTime( 'ddmmyy', Vencimento)                   +  // 121 a 126 - Data Vencimento
                  IntToStrZero( Round( ValorDocumento * 100 ), 13)        +  // 127 a 139 - Valo Titulo
                  IntToStrZero(fpNumero, 3)                               +  // 140 a 142 - Cód. Banco
                  StringOfChar('0',5) + PadRight(aEspecie,2) + 'N'        +  // 143 a 150 - Zeros + Especie do documento + Idntificação(valor fixo N)
                  FormatDateTime( 'ddmmyy', DataDocumento )               +  // 151 a 156 - Data de Emissão
                  StringOfChar('0',2)                                     +  // 157 a 158 - Zeros
                  StringOfChar('0',2)                                     +  // 159 a 160 - Zeros
                  IntToStrZero( Round(ValorMoraJuros * 100 ), 13)         +  // 161 a 173 - Valor a ser cobrado por dia de atraso
                  IfThen(DataDesconto < EncodeDate(2000,01,01),'000000',
                         FormatDateTime( 'ddmmyy', DataDesconto))         +  // 174 a 179 - Data limite para concessão desconto
                  IntToStrZero( round( ValorDesconto * 100 ), 13)         +  // 180 a 192 - Valor Desconto
                  IntToStrZero( round( ValorIOF * 100 ), 13)              +  // 193 a 205 - Valor IOF
                  IntToStrZero( round( ValorAbatimento * 100 ), 13)       +  // 206 a 218 - Valor Abatimento
                  TipoSacado + PadLeft(OnlyNumber(Sacado.CNPJCPF),14,'0') +  // 219 a 234 - Tipo de Inscrição + Número de Inscrição do Pagador
                  PadRight( Sacado.NomeSacado, 40, ' ')                   +  // 235 a 274 - Nome do Pagador
                  PadRight( Sacado.Logradouro + ' ' + Sacado.Numero + ' ' +
                    Sacado.Complemento, 40)                               +  // 275 a 314 - Complemento do sacado
                  PadRight( Sacado.Bairro, 12, ' ')                       +  // 315 a 326 - Bairro do sacado
                  PadRight( Sacado.CEP, 8 )                               +  // 327 a 334 - CEP do sacado
                  PadRight( Sacado.Cidade, 15, ' ')                       +  // 335 a 349 - Cidade do sacado
                  PadRight( Sacado.UF, 2, ' ')                            +  // 350 a 351 - UF do sacado
                  PadRight( Sacado.SacadoAvalista.NomeAvalista,30)        +  // 352 a 381 - Nome do sacador avalista
                  Space(10)                                               +  // 382 a 391 - Brancos
                  PadLeft( Protesto, 2, '0');                                // 392 a 392  Prazo

         wLinha:= wLinha + IntToStrZero(aRemessa.Count + 1, 7);              // Nº SEQÜENCIAL DO REGISTRO NO ARQUIVO
         wLinha := wLinha + DoMontaInstrucoes1;

         aRemessa.Text:= aRemessa.Text + UpperCase(wLinha);
      end;
   end;
end;

procedure TACBrBancoRendimento.GerarRegistroTrailler400( ARemessa:TStringList );
var
  wLinha: String;
begin
   wLinha := '9' + Space(393) + IntToStrZero( ARemessa.Count + 1, 6);// ID Registro // Contador de Registros
   ARemessa.Text:= ARemessa.Text + UpperCase(wLinha);
end;

Procedure TACBrBancoRendimento.LerRetorno400 ( ARetorno: TStringList );
var
  Titulo: TACBrTitulo;
  ContLinha: Integer;
  CodMotivo: Integer;
  rAgencia: String;
  rConta: String;
  Linha, rCedente, rCNPJCPF: String;
  rCodEmpresa: String;
begin
  if StrToIntDef(Copy(ARetorno.Strings[0], 77, 3), -1) <> fpNumero then
    raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
      ' não é um arquivo de retorno do ' + Nome));

  rCodEmpresa  := Trim(Copy(ARetorno[0], 27, 20)); // 027 a 046 - Código da Empresa
  rCedente     := Trim(Copy(ARetorno[0], 47, 30)); // 047 a 030 - Nome da Empresa
  rAgencia     := Trim(Copy(ARetorno[0], 27, ACBrBanco.TamanhoAgencia)); // 027 a 30 - Agencia
  rConta       := Trim(Copy(ARetorno[0], 31, ACBrBanco.TamanhoConta)); //  031 a 037 - Conta

  ACBrBanco.ACBrBoleto.NumeroArquivo := StrToIntDef(Copy(ARetorno[0],109,5),0); // 109 a 113 - Número Seqüencial

  ACBrBanco.ACBrBoleto.DataArquivo := StringToDateTimeDef(Copy(ARetorno[0],95,2)+'/'+
                                                         Copy(ARetorno[0],97,2)+'/'+
                                                         Copy(ARetorno[0],99,2),0, 'DD/MM/YY' ); // 095 a 100 - Data de Gravação

  case StrToIntDef(Copy(ARetorno[1], 2, 2), 0) of
    1: rCNPJCPF := Copy(ARetorno[1], 7, 11);
    2: rCNPJCPF := Copy(ARetorno[1], 4, 14);
  else
    rCNPJCPF := Copy(ARetorno[1], 4, 14);
  end;

  //
  with ACBrBanco.ACBrBoleto do
  begin
    if (not LeCedenteRetorno) and (rCodEmpresa <> PadLeft(Cedente.CodigoCedente, 14, '0')) then
      raise Exception.Create(ACBrStr('Código da Empresa do arquivo inválido.'));

    case StrToIntDef(Copy(ARetorno[1], 2, 2), 0) of
      1: Cedente.TipoInscricao := pFisica;
      2: Cedente.TipoInscricao := pJuridica;
    else
      Cedente.TipoInscricao := pJuridica;
    end;

    if LeCedenteRetorno then
    begin
      Cedente.CNPJCPF       := rCNPJCPF;
      Cedente.CodigoCedente := rCodEmpresa;
      Cedente.Nome          := rCedente;
      Cedente.Agencia       := rAgencia;
      Cedente.Conta         := rConta;
      //Cedente.ContaDigito   := 0; - NÃO EXISTENTE NO ARQUIVO DE RETORNO
    end;

    ACBrBanco.ACBrBoleto.ListadeBoletos.Clear;
  end;

  for ContLinha := 1 to ARetorno.Count - 2 do
  begin
    Linha := ARetorno[ContLinha];

    if Copy(Linha, 1, 1) <> '1' then
      Continue;

    Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

    with Titulo do
    begin
      SeuNumero               := Copy(Linha, 38, 25);
      NumeroDocumento         := Trim(Copy(Linha, 117, 10)); // 117 a 126 - Seu Número
      OcorrenciaOriginal.Tipo := CodOcorrenciaToTipo(StrToIntDef(Copy(Linha, 109, 2), 0)); // 109 a 110 - Cód. Ocorrência
      CodMotivo               := StrToIntDef(Copy(Linha, 378, 8), 0); // 378 a 385 - Retornos Erros

      if CodMotivo > 0 then
      begin
        MotivoRejeicaoComando.Add(Copy(Linha, 378, 8));
        DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo, CodMotivo));
      end;

      DataOcorrencia :=
        StringToDateTimeDef(
          Copy(Linha, 111, 2) + '/' +
          Copy(Linha, 113, 2) + '/'+
          Copy(Linha, 115, 2), 0, 'dd/mm/yy'); // 111 a 116 - Data de Ocorrência

      if Copy(Linha, 147, 2) <> '00' then
        Vencimento :=
          StringToDateTimeDef(
            Copy(Linha, 147, 2) + '/' +
            Copy(Linha, 149, 2) + '/'+
            Copy(Linha, 151, 2), 0, 'dd/mm/yy'); // 147 a 152 - Vencimento

      ValorDocumento       := StrToFloatDef(Copy(Linha, 153, 13), 0) / 100; //153 a 165 - Valor do Título
      ValorIOF             := StrToFloatDef(Copy(Linha, 215, 13), 0) / 100; //215 a 227 - Valor do I.O.F.
      ValorAbatimento      := StrToFloatDef(Copy(Linha, 228, 13), 0) / 100; //228 a 240 - Valor Abatimento
      ValorDesconto        := StrToFloatDef(Copy(Linha, 241, 13), 0) / 100; //241 a 253 - Valor do desconto concedido
      ValorMoraJuros       := StrToFloatDef(Copy(Linha, 267, 13), 0) / 100; //267 a 279 - Valor de mora pago pelo sacado
      ValorOutrosCreditos  := StrToFloatDef(Copy(Linha, 280, 13), 0) / 100; //280 a 376 - Zeros
      ValorRecebido        := StrToFloatDef(Copy(Linha, 254, 13), 0) / 100; //254 a 266 - Valor principal pago pelo sacado
      NossoNumero          := Copy(Linha, 63, 9);                           //063 a 073 - Nosso Numero
      Carteira             := Copy(Linha, 108, 1);                          //108 a 108 - carteira
      ValorDespesaCobranca := StrToFloatDef(Copy(Linha, 176, 13), 0) / 100; //176 a 188 - Tarifa de Cobrança

      if StrToIntDef(Copy(Linha, 296, 6), 0) <> 0 then
        DataCredito :=
          StringToDateTimeDef(
            Copy(Linha, 386, 2) + '/' +
            Copy(Linha, 388, 2) + '/' +
            Copy(Linha, 400, 2), 0, 'dd/mm/yy'); //386 a 391 - Data Gravação/Data do Crédito

    end;
  end;
end;

function TACBrBancoRendimento.TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String;
var
  CodOcorrencia: Integer;
begin
  CodOcorrencia := StrToIntDef(TipoOcorrenciaToCod(TipoOcorrencia), 0);

  case CodOcorrencia of
    02: Result := '02-Entrada Confirmada';
    03: Result := '03-Entrada Rejeitada';
    05: Result := '05-Campo Livre Alterado';
    06: Result := '06-Liquidacao Normal';
    08: Result := '08-Liquidação em Cartório';
    09: Result := '09-Baixa Automática';
    10: Result := '10-Baixa por ter sido liquidado';
    12: Result := '12-Confirma Abatimento';
    13: Result := '13-Abatimento Cancelado';
    14: Result := '14-Vencimento Alterado';
    15: Result := '15-Baixa Rejeitada';
    16: Result := '16-Instrução Rejeitada';
    19: Result := '19-Confirma Recebimento de Ordem de Protesto';
    20: Result := '20-Confirma Recebimento de Ordem de Sustação';
    22: Result := '22-Seu número alterado';
    23: Result := '23-Título enviado para cartório';
    24: Result := '24-Confirma recebimento de ordem de não protestar';
    28: Result := '28-Débito de Tarifas/Custas – Correspondentes';
    40: Result := '40-Tarifa de Entrada (debitada na Liquidação) ';
    43: Result := '43-Baixado por ter sido protestado';
    96: Result := '96-Tarifa Sobre Instruções – Mês anterior';
    97: Result := '97-Tarifa Sobre Baixas – Mês Anterior';
    98: Result := '98-Tarifa Sobre Entradas – Mês Anterior';
    99: Result := '99-Tarifa Sobre Instruções de Protesto/Sustação – Mês Anterior';
  end;
end;

function TACBrBancoRendimento.CodOcorrenciaToTipo(const CodOcorrencia:
   Integer ) : TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02: Result := toRetornoRegistroConfirmado;
    03: Result := toRetornoRegistroRecusado;
    06: Result := toRetornoLiquidado;
    07: Result := toRetornoLiquidadoParcialmente;
    09: Result := toRetornoBaixaAutomatica;
    10: Result := toRetornoBaixadoInstAgencia;
    11: Result := toRetornoTituloEmSer;
    12: Result := toRetornoAbatimentoConcedido;
    13: Result := toRetornoAbatimentoCancelado;
    14: Result := toRetornoVencimentoAlterado;
    15: Result := toRetornoLiquidadoEmCartorio;
    16: Result := toRetornoBaixadoFrancoPagamento;
    19: Result := toRetornoRecebimentoInstrucaoProtestar;
    20: Result := toRetornoRecebimentoInstrucaoSustarProtesto;
    23: Result := toRetornoEncaminhadoACartorio;
    40: Result := toRetornoBaixaPorProtesto;
    41: Result := toRetornoLiquidadoAposBaixaOuNaoRegistro;
    42: Result := toRetornoRetiradoDeCartorio;
    43: Result := toRetornoDespesasProtesto;
  else
    Result := toRetornoOutrasOcorrencias;
  end;
end;

function TACBrBancoRendimento.TipoOcorrenciaToCod ( const TipoOcorrencia: TACBrTipoOcorrencia ) : String;
begin
  case TipoOcorrencia of
    toRetornoRegistroConfirmado                : Result := '02';
    toRetornoRegistroRecusado                  : Result := '03';
    toRetornoLiquidado                         : Result := '06';
    toRetornoLiquidadoParcialmente             : Result := '07';
    toRetornoBaixaAutomatica                   : Result := '09';
    toRetornoBaixadoInstAgencia                : Result := '10';
    toRetornoTituloEmSer                       : Result := '11';
    toRetornoAbatimentoConcedido               : Result := '12';
    toRetornoAbatimentoCancelado               : Result := '13';
    toRetornoVencimentoAlterado                : Result := '14';
    toRetornoLiquidadoEmCartorio               : Result := '15';
    toRetornoBaixadoFrancoPagamento            : Result := '16';
    toRetornoRecebimentoInstrucaoProtestar     : Result := '19';
    toRetornoRecebimentoInstrucaoSustarProtesto: Result := '20';
    toRetornoEncaminhadoACartorio              : Result := '23';
    toRetornoBaixaPorProtesto                  : Result := '40';
    toRetornoLiquidadoAposBaixaOuNaoRegistro   : Result := '41';
    toRetornoRetiradoDeCartorio                : Result := '42';
    toRetornoDespesasProtesto                  : Result := '43';
  else
    Result := '02';
  end;
end;

function TACBrBancoRendimento.COdMotivoRejeicaoToDescricao( const TipoOcorrencia:TACBrTipoOcorrencia ;CodMotivo: Integer) : String;
begin
  if TipoOcorrencia = toRetornoRegistroRecusado then
  begin
   // Entradas Rejeitadas – Ocorrência 03
    case CodMotivo of
      03: Result := '03-CEP inválido – Não temos cobrador – Cobrador não Localizado';
      04: Result := '04-Sigla do Estado inválida';
      05: Result := '05-Data de Vencimento inválida ou fora do prazo mínimo';
      06: Result := '06-Código do Banco inválido';
      08: Result := '08-Nome do sacado não informado';
      10: Result := '10-Logradouro não informado ';
      14: Result := '14-Registro em duplicidade';
      19: Result := '19-Data de desconto inválida ou maior que a data de vencimento';
      20: Result := '20-Valor de IOF não numérico';
      21: Result := '21-Movimento para título não cadastrado no sistema';
      22: Result := '22-Valor de desconto + abatimento maior que o valor do título';
      25: Result := '25-CNPJ ou CPF do sacado inválido (aceito com restrições)';
      26: Result := '26-Espécie de documento inválida';
      27: Result := '27-Data de emissão do título inválida';
      28: Result := '28-Seu número não informado';
      29: Result := '29-CEP é igual a espaço ou zeros; ou não numérico';
      30: Result := '30-Valor do título não numérico ou inválido';
      36: Result := '36-Valor de permanência (mora) não numérico';
      37: Result := '37-Valor de permanência inconsistente, pois, dentro de um mês, será maior que o valor do título';
      38: Result := '38-Valor de desconto/abatimento não numérico ou inválido';
      39: Result := '39-Valor de abatimento não numérico';
      42: Result := '42-Título já existente em nossos registros. Nosso número não aceito';
      43: Result := '43-Título enviado em duplicidade nesse movimento';
      44: Result := '44-Título zerado ou em branco; ou não numérico na remessa';
      46: Result := '46-Título enviado fora da faixa de Nosso Número, estipulada para o cliente';
      51: Result := '51-Tipo/Número de Inscrição Sacador/Avalista Inválido';
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
      Result := IntToStrZero(CodMotivo, 3) + ' - Outros Motivos';
    end;
  end else if TipoOcorrencia = toRetornoLiquidadoEmCartorio then
  begin
    //Baixas Rejeitadas – Ocorrência 15
    case CodMotivo of
      05: Result := '05-Solicitação de baixa para título já baixado ou liquidado';
      06: Result := '06-Solicitação de baixa para título não registrado no sistema';
      08: Result := '08-Solicitação de baixa para título em float';
    else
      Result := IntToStrZero(CodMotivo, 3) + ' - Outros Motivos';
    end;
  end else if TipoOcorrencia = toRetornoBaixadoFrancoPagamento then
  begin
    //Instruções Rejeitadas – Ocorrência 16
    case CodMotivo of
      04: Result := '04-Data de vencimento não numérica ou inválida';
      05: Result := '05-Data de Vencimento inválida ou fora do prazo mínimo';
      14: Result := '14-Registro em duplicidade';
      19: Result := '19-Data de desconto inválida ou maior que a data de vencimento';
      20: Result := '20-Campo livre não informado';
      21: Result := '21-Título não registrado no sistema';
      22: Result := '22-Título baixado ou liquidado';
      26: Result := '26-Espécie de documento inválida ';
      27: Result := '27-Instrução não aceita, por não ter sido emitida ordem de protesto ao cartório';
      28: Result := '28-Título tem instrução de cartório ativa';
      29: Result := '29-Título não tem instrução de carteira ativa';
      30: Result := '30-Existe instrução de não protestar, ativa para o título';
      36: Result := '36-Valor de permanência (mora) não numérico';
      37: Result := '37-Título Descontado – Instrução não permitida para a carteira';
      38: Result := 'Valor do abatimento não numérico ou maior que a soma do valor do título + permanência + multa';
      39: Result := '39-Título em cartório';
      40: Result := '40-Instrução recusada – Reprovado no Represamento para Análise';
      44: Result := '44-Título zerado ou em branco; ou não numérico na remessa ';
      51: Result := '51-Tipo/Número de Inscrição Sacador/Avalista Inválido';
      53: Result := '53-Prazo de vencimento do título excede ao da contratação';
      57: Result := '57-Remessa contendo duas instruções incompatíveis – não protestar e dias de protesto ou prazo para protesto inválido';
      99: Result := '99-Ocorrência desconhecida na remessa';
    else
      Result := IntToStrZero(CodMotivo, 3) + ' - Outros Motivos';
    end;
  end;
end;

function TACBrBancoRendimento.CodOcorrenciaToTipoRemessa(const CodOcorrencia:Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02 : Result:= toRemessaBaixar;                          {Pedido de Baixa}
    04 : Result:= toRemessaConcederAbatimento;              {Concessão de Abatimento}
    05 : Result:= toRemessaCancelarAbatimento;              {Cancelamento de Abatimento}
    06 : Result:= toRemessaAlterarVencimento;               {Alteração de vencimento}
    07 : Result:= toRemessaAlterarControleParticipante;     {Alteração de Campo Livre       (não disponível) }
    08 : Result:= toRemessaAlterarNumeroControle;           {Alteração de Seu Número       (não disponível) }
    09 : Result:= toRemessaProtestar;                       {Protestar }
    10 : Result:= toRemessaNaoProtestar;                    {Pedido de Não Protestar }
    18 : Result:= toRemessaCancelarInstrucaoProtestoBaixa;  {Sustar Protesto }
    47 : Result:= toRemessaAlterarValorTitulo;              {Alteração do Valor Nominal do título (altera vencimento também) (para produtos que permitem esta instrução)  }
  else
     Result:= toRemessaRegistrar;                           {Remessa}
  end;
end;


end.


