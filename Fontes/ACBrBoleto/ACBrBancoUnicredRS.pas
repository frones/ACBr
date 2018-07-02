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

{$I ACBr.inc}

unit ACBrBancoUnicredRS;

interface

uses
  Classes, SysUtils, ACBrBoleto;

type

  { TACBrBancoUnicredRS }

  TACBrBancoUnicredRS = class(TACBrBancoClass)
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
    function TipoOCorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia):String; override;
    function CodMotivoRejeicaoToDescricao(const TipoOcorrencia:TACBrTipoOcorrencia; CodMotivo:Integer): String; override;

    function CodOcorrenciaToTipoRemessa(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
  end;

implementation

uses {$IFDEF COMPILER6_UP} dateutils {$ELSE} ACBrD5 {$ENDIF},
  StrUtils,
  ACBrUtil ;

{ TACBrBancoUnicredRS }

constructor TACBrBancoUnicredRS.create(AOwner: TACBrBanco);
begin
   inherited create(AOwner);
   fpDigito                 := 4;
   fpNome                   := 'Unicred';
   fpNumero                 := 091;
   fpTamanhoMaximoNossoNum  := 10;
   fpTamanhoAgencia         := 4;
   fpTamanhoConta           := 7;
   fpTamanhoCarteira        := 2;
end;

function TACBrBancoUnicredRS.CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo ): String;
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

function TACBrBancoUnicredRS.MontarCodigoBarras ( const ACBrTitulo: TACBrTitulo) : String;
var
  sCodigoBarras   : String;
  sFatorVencimento: String;
  sDigitoCodBarras: String;
begin
   with ACBrTitulo.ACBrBoleto do
   begin
      sFatorVencimento  := CalcularFatorVencimento(ACBrTitulo.Vencimento);

      sCodigoBarras     := IntToStrZero(Numero, 3) +                                              { Instituição Financ }
                           '9' +                                                                  { Cód. Moeda = Real }
                           sFatorVencimento +                                                     { Fator Vencimento }
                           IntToStrZero(Round(ACBrTitulo.ValorDocumento * 100), 10) +             { Valor Nominal }
                           PadLeft(OnlyNumber(Cedente.Agencia), fpTamanhoAgencia, '0') +          { Campo Livre 1 - Agência }
                           PadLeft(RightStr(Cedente.Conta + Cedente.ContaDigito, 10), 10, '0') +  { Campo Livre 2 - Conta }
                           ACBrTitulo.NossoNumero +                                               { Campo Livre 3 - Nosso Núm }
                           CalcularDigitoVerificador(ACBrTitulo);                                 {                C/ Dígito }

      sDigitoCodBarras  := CalcularDigitoCodigoBarras(sCodigoBarras);
   end;

   { Insere o digito verificador calculado no código de barras }
   Result := IntToStrZero(Numero, 3) + '9' + sDigitoCodBarras + Copy(sCodigoBarras, 5, 39);
end;

function TACBrBancoUnicredRS.MontarCampoNossoNumero (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
   Result := ACBrTitulo.Carteira + '/'+ ACBrTitulo.NossoNumero + '-'+ CalcularDigitoVerificador(ACBrTitulo);
end;

function TACBrBancoUnicredRS.MontarCampoCodigoCedente (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
   Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia        + '-' +
             ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito  + '/' +
             ACBrTitulo.ACBrBoleto.Cedente.Conta          + '-' +
             ACBrTitulo.ACBrBoleto.Cedente.ContaDigito;
end;

procedure TACBrBancoUnicredRS.GerarRegistroHeader400(NumeroRemessa : Integer; ARemessa:TStringList);
var
  wLinha: String;
begin
   with ACBrBanco.ACBrBoleto.Cedente do
   begin
      wLinha:= '0'                                                +  { ID do Registro }
               '1'                                                +  { ID do Arquivo( 1 - Remessa) }
               'REMESSA'                                          +  { Literal de Remessa }
               '01'                                               +  { Código do Tipo de Serviço }
               PadRight('COBRANCA', 15)                           +  { Descrição do tipo de serviço }
               PadLeft(CodigoCedente, 20, '0')                    +  { Codigo da Empresa no Banco }
               PadRight(Nome, 30)                                 +  { Nome da Empresa }
               IntToStrZero(Numero, 3) + PadRight('UNICRED', 15)  +  { Código e Nome do Banco(091 - Unicred ) }
               FormatDateTime('ddmmyy',Now) + Space(07)           +  { Data de geração do arquivo + brancos }
               '113'                                              +  { Cód. Parâm. Movto da Unicred }
               IntToStrZero(NumeroRemessa, 7) + Space(277)        +  { Nr. Sequencial de Remessa + brancos }
               IntToStrZero(1, 6);                                   { Nr. Sequencial de Remessa + brancos + Contador }

      ARemessa.Text:= ARemessa.Text + UpperCase(wLinha);
   end;
end;

procedure TACBrBancoUnicredRS.GerarRegistroTransacao400(ACBrTitulo :TACBrTitulo; aRemessa: TStringList);
var
  sDigitoNossoNumero, sOcorrencia, sAgencia           : String;
  sProtesto, sTipoSacado, sConta                      : String;
  sCarteira, sLinha, sNossoNumero, sNumContrato       : String;
  iTamNossoNum: Integer;

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

        Result := sLineBreak +
                  '2'               +                                    // IDENTIFICAÇÃO DO LAYOUT PARA O REGISTRO
                  Copy(PadRight(Mensagem[1], 80, ' '), 1, 80);               // CONTEÚDO DA 1ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO

        if Mensagem.Count >= 3 then
           Result := Result +
                     Copy(PadRight(Mensagem[2], 80, ' '), 1, 80)              // CONTEÚDO DA 2ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO
        else
           Result := Result + PadRight('', 80, ' ');                          // CONTEÚDO DO RESTANTE DAS LINHAS

        if Mensagem.Count >= 4 then
           Result := Result +
                     Copy(PadRight(Mensagem[3], 80, ' '), 1, 80)              // CONTEÚDO DA 3ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO
        else
           Result := Result + PadRight('', 80, ' ');                          // CONTEÚDO DO RESTANTE DAS LINHAS

        if Mensagem.Count >= 5 then
           Result := Result +
                     Copy(PadRight(Mensagem[4], 80, ' '), 1, 80)              // CONTEÚDO DA 4ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO
        else
           Result := Result + PadRight('', 80, ' ');                          // CONTEÚDO DO RESTANTE DAS LINHAS


        Result := Result                                              +
                  space(45)                                           +  // COMPLEMENTO DO REGISTRO
                  sCarteira                                           +
                  sAgencia                                            +
                  sConta                                              +
                  Cedente.ContaDigito                                 +
                  NossoNumero + sDigitoNossoNumero                    +
                  IntToStrZero( aRemessa.Count + 2, 6);                  // Nº SEQÜENCIAL DO REGISTRO NO ARQUIVO
     end;
  end;

begin

   with ACBrTitulo do
   begin
      sNossoNumero := ACBrTitulo.NossoNumero;
      iTamNossoNum := CalcularTamMaximoNossoNumero(ACBrTitulo.Carteira,
                                                   ACBrTitulo.NossoNumero);

      if (ACBrBoleto.Cedente.ResponEmissao = tbBancoEmite) then
      begin
        sNossoNumero      := StringOfChar('0', iTamNossoNum);
        sDigitoNossoNumero := '0';
      end
      else
      begin
        sNossoNumero      := ACBrTitulo.NossoNumero;
        sDigitoNossoNumero := CalcularDigitoVerificador(ACBrTitulo);
      end;

      sAgencia      := IntToStrZero(StrToIntDef(OnlyNumber(ACBrBoleto.Cedente.Agencia), 0), 5);
      sConta        := IntToStrZero(StrToIntDef(OnlyNumber(ACBrBoleto.Cedente.Conta)  , 0), 7);
      sCarteira     := IntToStrZero(StrToIntDef(trim(Carteira), 0), 3);
      sNumContrato  := sAgencia + sConta + ACBrBoleto.Cedente.ContaDigito;

      {Pegando Código da Ocorrencia}
      case OcorrenciaOriginal.Tipo of
         toRemessaBaixar                         : sOcorrencia := '02'; {Pedido de Baixa}
         toRemessaConcederAbatimento             : sOcorrencia := '04'; {Concessão de Abatimento}
         toRemessaCancelarAbatimento             : sOcorrencia := '05'; {Cancelamento de Abatimento concedido}
         toRemessaAlterarVencimento              : sOcorrencia := '06'; {Alteração de vencimento}
         toRemessaAlterarNumeroControle          : sOcorrencia := '08'; {Alteração de seu número}
         toRemessaProtestar                      : sOcorrencia := '09'; {Pedido de protesto}
         toRemessaNaoProtestar                   : sOcorrencia := '10'; {Não Protestar}
         toRemessaCancelarInstrucaoProtestoBaixa : sOcorrencia := '18'; {Sustar protesto e baixar}
         toRemessaCancelarInstrucaoProtesto      : sOcorrencia := '19'; {Sustar protesto e manter na carteira}
         toRemessaOutrasOcorrencias              : sOcorrencia := '31'; {Alteração de Outros Dados}
      else
         sOcorrencia := '01';                                           {Remessa}
      end;

      {Pegando Tipo de Boleto}
      if not(ACBrBoleto.Cedente.ResponEmissao = tbCliEmite) then
        if (NossoNumero = EmptyStr) then
           sDigitoNossoNumero := '0';

      {Pegando campo Intruções}
      if (DataProtesto > 0) and (DataProtesto > Vencimento) then
         sProtesto := '06' + IntToStrZero(DaysBetween(DataProtesto, Vencimento), 2)
      else if sOcorrencia = '31' then
         sProtesto := '9999'
      else
         sProtesto := '0000';

      {Pegando Tipo de Sacado}
      case Sacado.Pessoa of
         pFisica   : sTipoSacado := '01';
         pJuridica : sTipoSacado := '02';
      else
         sTipoSacado := '99';
      end;

      with ACBrBoleto do
      begin
         sLinha:= '1'                                                     +  { 001: ID Registro }
                  sAgencia                                                +  { 002: Agência }
                  Cedente.AgenciaDigito                                   +  { 007: Agência Dígito }
                  PadLeft(sConta, 12, '0')                                +  { 008: Conta Corrente }
                  Cedente.ContaDigito                                     +  { 020: Conta Corrente Dígito }
                  '0' + sCarteira                                         +  { 021: Zero + Carteira }
                  sNumContrato                                            +  { 025: Número Contrato }
                  PadRight(SeuNumero, 25, ' ')                            +  { 038: Numero Controle do Participante }
                  IntToStrZero(Numero, 3) + '00'                          +  { 063: Cód. do Banco = 091 }
                  PadLeft(sNossoNumero + sDigitoNossoNumero, 15, '0')     +  { 068: Nosso Número }
                  IntToStrZero(Round(ValorDescontoAntDia * 100), 10)      +  { 083: Desconto Bonificação por dia }
                  '0' + Space(12) + '0' + Space(2)                        +  { 093: Zero | 094: Branco |
                                                                               095: 10 Brancos | 105: Branco |
                                                                               106: Zeros | 107: 2 Brancos }
                  sOcorrencia                                             +  { 109: Ocorrência }
                  PadRight(NumeroDocumento, 10)                           +  { 111: Número DOcumento }
                  FormatDateTime( 'ddmmyy', Vencimento)                   +  { 121: Vencimento }
                  IntToStrZero(Round(ValorDocumento * 100 ), 13)          +  { 127: Valor do Título }
                  StringOfChar('0', 11)                                   +  { 140: Zeros }
                  FormatDateTime('ddmmyy', DataDocumento)                 +  { 151: Data de Emissão }
                  sProtesto                                               +  { 157: Protesto }
                  IntToStrZero(Round(ValorMoraJuros * 100), 13)           +  { 161: Valor por dia de Atraso }
                  IfThen(DataDesconto < EncodeDate(2000,01,01), '000000',
                         FormatDateTime('ddmmyy', DataDesconto))         +  { 174: Data Limite Desconto }
                  IntToStrZero(Round(ValorDesconto * 100), 13)           +  { 180: Valor Desconto }
                  sNossoNumero + sDigitoNossoNumero + '00'               +  { 193: Nosso Número na Unicred }
                  IntToStrZero(Round(ValorAbatimento * 100), 13)         +  { 206: Valor Abatimento }
                  sTipoSacado                                            +  { 219: Tipo Inscrição Sacado }
                  PadLeft(OnlyNumber(Sacado.CNPJCPF), 14, '0')           +  { 221: Núm. Incrição Sacado }
                  PadRight(Sacado.NomeSacado, 40, ' ')                   +  { 235: Nome do Sacado }
                  PadRight(Sacado.Logradouro + ' ' + Sacado.Numero, 40)  +  { 275: Endereço do Sacado }
                  PadRight(Sacado.Bairro, 12, ' ')                       +  { 315: Bairro do Sacado }
                  PadRight(Sacado.CEP, 8, ' ')                           +  { 327: CEP do Sacado }
                  PadRight(Sacado.Cidade, 20, ' ')                       +  { 335: Cidade do Sacado }
                  PadRight(Sacado.UF, 2, ' ')                            +  { 355: UF Cidade do Sacado }
                  Space(38)                                              +  { 357: Sacador/Avalista }
                  IntToStrZero(aRemessa.Count + 1, 6);                      { 395: Núm Sequencial arquivo }

         sLinha := sLinha + DoMontaInstrucoes1;

         aRemessa.Text := aRemessa.Text + UpperCase(sLinha);
      end;
   end;
end;

procedure TACBrBancoUnicredRS.GerarRegistroTrailler400( ARemessa:TStringList );
var
  wLinha: String;
begin
   wLinha := '9' + Space(393)                     +  { ID Registro }
             IntToStrZero( ARemessa.Count + 1, 6);   { Contador de Registros }

   ARemessa.Text:= ARemessa.Text + UpperCase(wLinha);
end;

Procedure TACBrBancoUnicredRS.LerRetorno400 ( ARetorno: TStringList );
var
  vTitulo                     : TACBrTitulo;
  iContLinha, iCodOcorrencia  : Integer;
  iCodMotivo, i, iMotivoLinha : Integer;
  sCodMotivo_19, sAgencia     : String;
  sConta, sDigitoConta        : String;
  sLinha, sCedente, sCNPJCPF  : String;
  sCodEmpresa                 : String;
begin
   if (StrToIntDef(Copy(ARetorno.Strings[0], 77, 3), -1) <> Numero) then
      raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                             'não é um arquivo de retorno da ' + Nome));

   sCodEmpresa  := Trim(Copy(ARetorno[0], 27, 20));
   sCedente     := Trim(Copy(ARetorno[0], 47, 30));
   sAgencia     := Trim(Copy(ARetorno[1], 25, ACBrBanco.TamanhoAgencia));
   sConta       := Trim(Copy(ARetorno[1], 30, ACBrBanco.TamanhoConta));
   sDigitoConta :=      Copy(ARetorno[1], 42, 1);

   ACBrBanco.ACBrBoleto.NumeroArquivo := 0;//StrToIntDef(Copy(ARetorno[0], 109, 5), 0);

   ACBrBanco.ACBrBoleto.DataArquivo := StringToDateTimeDef(Copy(ARetorno[0], 95, 2) + '/' +
                                                           Copy(ARetorno[0], 97, 2) + '/' +
                                                           Copy(ARetorno[0], 99, 2), 0, 'DD/MM/YY' );

   ACBrBanco.ACBrBoleto.DataCreditoLanc := StringToDateTimeDef(Copy(ARetorno[0], 380, 2) + '/' +
                                                               Copy(ARetorno[0], 382, 2) + '/' +
                                                               Copy(ARetorno[0], 384, 2), 0, 'DD/MM/YY');

   case StrToIntDef(Copy(ARetorno[1],2,2),0) of
      11: sCNPJCPF := Copy(ARetorno[1],7,11);
      14: sCNPJCPF := Copy(ARetorno[1],4,14);
   else
     sCNPJCPF := Copy(ARetorno[1],4,14);
   end;

   ValidarDadosRetorno(sAgencia, sConta);
   with ACBrBanco.ACBrBoleto do
   begin
      if (not LeCedenteRetorno) and (sCodEmpresa <> PadLeft(Cedente.CodigoCedente, 20, '0')) then
         raise Exception.Create(ACBrStr('Código da Empresa do arquivo inválido'));

      case StrToIntDef(Copy(ARetorno[1],2,2),0) of
         11: Cedente.TipoInscricao:= pFisica;
         14: Cedente.TipoInscricao:= pJuridica;
      else
         Cedente.TipoInscricao := pJuridica;
      end;

      if LeCedenteRetorno then
      begin
         try
           Cedente.CNPJCPF := sCNPJCPF;
         except
           // Retorno quando é CPF está vindo errado por isso ignora erro na atribuição
         end;

         Cedente.CodigoCedente:= sCodEmpresa;
         Cedente.Nome         := sCedente;
         Cedente.Agencia      := sAgencia;
         Cedente.AgenciaDigito:= '0';
         Cedente.Conta        := sConta;
         Cedente.ContaDigito  := sDigitoConta;
      end;

      ACBrBanco.ACBrBoleto.ListadeBoletos.Clear;
   end;

   for iContLinha := 1 to ARetorno.Count - 2 do
   begin
      sLinha := ARetorno[iContLinha] ;

      if Copy(sLinha,1,1)<> '1' then
         Continue;

      vTitulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

      with vTitulo do
      begin
         SeuNumero                   := copy(sLinha,38,25);
         NumeroDocumento             := copy(sLinha,117,10);
         OcorrenciaOriginal.Tipo     := CodOcorrenciaToTipo(StrToIntDef(
                                        copy(sLinha,109,2),0));

         iCodOcorrencia := StrToInt(IfThen(copy(sLinha,109,2) = '00','00',copy(sLinha,109,2)));

         //-|Se a ocorrencia for igual a 19 - Confirmação de Receb. de Protesto
         //-|Verifica o motivo na posição 295 - A = Aceite , D = Desprezado
         if(iCodOcorrencia = 19)then
          begin
            sCodMotivo_19:= copy(sLinha,295,1);
            if(sCodMotivo_19 = 'A')then
             begin
               MotivoRejeicaoComando.Add(copy(sLinha,295,1));
               DescricaoMotivoRejeicaoComando.Add('A - Aceito');
             end
            else
             begin
               MotivoRejeicaoComando.Add(copy(sLinha,295,1));
               DescricaoMotivoRejeicaoComando.Add('D - Desprezado');
             end;
          end
         else
          begin
            iMotivoLinha := 319;
            for i := 0 to 4 do
            begin
               iCodMotivo := StrToInt(IfThen(copy(sLinha,iMotivoLinha,2) = '00','00',copy(sLinha,iMotivoLinha,2)));

               {Se for o primeiro motivo}
               if (i = 0) then
                begin
                  {Somente estas ocorrencias possuem motivos 00}
                  if(iCodOcorrencia in [02, 06, 09, 10, 15, 17])then
                   begin
                     MotivoRejeicaoComando.Add(IfThen(copy(sLinha,iMotivoLinha,2) = '00','00',copy(sLinha,iMotivoLinha,2)));
                     DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo,iCodMotivo));
                   end
                  else
                   begin
                     if(iCodMotivo = 0)then
                      begin
                        MotivoRejeicaoComando.Add('00');
                        DescricaoMotivoRejeicaoComando.Add('Sem Motivo');
                      end
                     else
                      begin
                        MotivoRejeicaoComando.Add(IfThen(copy(sLinha,iMotivoLinha,2) = '00','00',copy(sLinha,iMotivoLinha,2)));
                        DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo,iCodMotivo));
                      end;
                   end;
                end
               else
                begin
                  //Apos o 1º motivo os 00 significam que não existe mais motivo
                  if iCodMotivo <> 0 then
                  begin
                     MotivoRejeicaoComando.Add(IfThen(copy(sLinha,iMotivoLinha,2) = '00','00',copy(sLinha,iMotivoLinha,2)));
                     DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo,iCodMotivo));
                  end;
                end;

               iMotivoLinha := iMotivoLinha + 2; //Incrementa a coluna dos motivos
            end;
          end;

         DataOcorrencia := StringToDateTimeDef( Copy(sLinha,111,2)+'/'+
                                                Copy(sLinha,113,2)+'/'+
                                                Copy(sLinha,115,2),0, 'DD/MM/YY' );
         if Copy(sLinha,147,2)<>'00' then
            Vencimento := StringToDateTimeDef( Copy(sLinha,147,2)+'/'+
                                               Copy(sLinha,149,2)+'/'+
                                               Copy(sLinha,151,2),0, 'DD/MM/YY' );

         ValorDocumento       := StrToFloatDef(Copy(sLinha,153,13),0)/100;
         ValorIOF             := StrToFloatDef(Copy(sLinha,215,13),0)/100;
         ValorAbatimento      := StrToFloatDef(Copy(sLinha,228,13),0)/100;
         ValorDesconto        := StrToFloatDef(Copy(sLinha,241,13),0)/100;
         ValorMoraJuros       := StrToFloatDef(Copy(sLinha,267,13),0)/100;
         ValorOutrosCreditos  := StrToFloatDef(Copy(sLinha,280,13),0)/100;
         ValorRecebido        := StrToFloatDef(Copy(sLinha,254,13),0)/100;
         NossoNumero          := Copy(sLinha,71,11);
         Carteira             := Copy(sLinha,22,3);
         ValorDespesaCobranca := StrToFloatDef(Copy(sLinha,176,13),0)/100;
         ValorOutrasDespesas  := StrToFloatDef(Copy(sLinha,189,13),0)/100;

         if StrToIntDef(Copy(sLinha,296,6),0) <> 0 then
            DataCredito:= StringToDateTimeDef( Copy(sLinha,296,2)+'/'+
                                               Copy(sLinha,298,2)+'/'+
                                               Copy(sLinha,300,2),0, 'DD/MM/YY' );
      end;
   end;
end;

function TACBrBancoUnicredRS.TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String;
var
  CodOcorrencia: Integer;
begin
  Result        := EmptyStr;
  CodOcorrencia := StrToIntDef(TipoOCorrenciaToCod(TipoOcorrencia),0);

  case CodOcorrencia of
    02: Result := '02-Entrada Confirmada';
    03: Result := '03-Entrada Rejeitada';
    06: Result := '06-Liquidação Normal';
    09: Result := '09-Baixado Automaticamente via Arquivo';
    10: Result := '10-Baixado conforme instruções da Agência';
    12: Result := '12-Abatimento Concedido';
    13: Result := '13-Abatimento Cancelado';
    14: Result := '14-Vencimento Alterado';
    15: Result := '15-Liquidação em Cartório';
    19: Result := '19-Confirmação Recebimento Instrução de Protesto';
    20: Result := '20-Confirmação Recebimento Instrução Sustação de Protesto';
    21: Result := '21-Confirma Recebimento de Instrução de Não Protestar';
    24: Result := '24-Entrada rejeitada por CEP Irregular';
    27: Result := '27-Baixa Rejeitada';
    30: Result := '30-Alteração de Outros Dados Rejeitados';
    32: Result := '32-Instrução Rejeitada';
    33: Result := '33-Confirmação Pedido Alteração Outros Dados';
  end;
end;

function TACBrBancoUnicredRS.CodOcorrenciaToTipo(const CodOcorrencia:
   Integer ) : TACBrTipoOcorrencia;
begin

  case CodOcorrencia of
    02: Result := toRetornoRegistroConfirmado;
    03: Result := toRetornoRegistroRecusado;
    06: Result := toRetornoLiquidado;
    09: Result := toRetornoBaixadoViaArquivo;
    10: Result := toRetornoBaixadoInstAgencia;
    12: Result := toRetornoAbatimentoConcedido;
    13: Result := toRetornoAbatimentoCancelado;
    14: Result := toRetornoVencimentoAlterado;
    15: Result := toRetornoLiquidadoEmCartorio;
    19: Result := toRetornoRecebimentoInstrucaoProtestar;
    20: Result := toRetornoRecebimentoInstrucaoSustarProtesto;
    21: Result := toRetornoAcertoControleParticipante;
    24: Result := toRetornoEntradaRejeitaCEPIrregular;
    27: Result := toRetornoBaixaRejeitada;
    30: Result := toRetornoAlteracaoOutrosDadosRejeitada;
    32: Result := toRetornoInstrucaoRejeitada;
    33: Result := toRetornoRecebimentoInstrucaoAlterarDados;
  else
    Result := toRetornoOutrasOcorrencias;
  end;
end;

function TACBrBancoUnicredRS.TipoOcorrenciaToCod ( const TipoOcorrencia: TACBrTipoOcorrencia ) : String;
begin
  Result := '';

  case TipoOcorrencia of
    toRetornoRegistroConfirmado                             : Result := '02';
    toRetornoRegistroRecusado                               : Result := '03';
    toRetornoLiquidado                                      : Result := '06';
    toRetornoBaixadoViaArquivo                              : Result := '09';
    toRetornoBaixadoInstAgencia                             : Result := '10';
    toRetornoAbatimentoConcedido                            : Result := '12';
    toRetornoAbatimentoCancelado                            : Result := '13';
    toRetornoVencimentoAlterado                             : Result := '14';
    toRetornoLiquidadoEmCartorio                            : Result := '15';
    toRetornoRecebimentoInstrucaoProtestar                  : Result := '19';
    toRetornoRecebimentoInstrucaoSustarProtesto             : Result := '20';
    toRetornoAcertoControleParticipante                     : Result := '21';
    toRetornoEntradaRejeitaCEPIrregular                     : Result := '24';
    toRetornoBaixaRejeitada                                 : Result := '27';
    toRetornoAlteracaoOutrosDadosRejeitada                  : Result := '30';
    toRetornoInstrucaoRejeitada                             : Result := '32';
    toRetornoRecebimentoInstrucaoAlterarDados               : Result := '33';
  else
    Result := '02';
  end;
end;

function TACBrBancoUnicredRS.COdMotivoRejeicaoToDescricao( const TipoOcorrencia:TACBrTipoOcorrencia ;CodMotivo: Integer) : String;
begin
   case TipoOcorrencia of
     toRetornoRegistroConfirmado:
       case CodMotivo  of
         00: Result := '00-Ocorrencia aceita';
       else
         Result := IntToStrZero(CodMotivo,2) +' - Outros Motivos';
       end;
    toRetornoRegistroRecusado:
      case CodMotivo of
         02: Result := '02-Codigo do registro detalhe invalido';
         03: Result := '03-Codigo da Ocorrencia Invalida';
         04: Result := '04-Codigo da Ocorrencia nao permitida para a carteira';
         05: Result := '05-Codigo de Ocorrencia nao numerico';
         07: Result := '07-Agencia\Conta\Digito invalido';
         08: Result := '08-Nosso numero invalido';
         09: Result := '09-Nosso numero duplicado';
         10: Result := '10-Carteira invalida';
         16: Result := '16-Data de vencimento invalida';
         18: Result := '18-Vencimento fora do prazo de operacao';
         20: Result := '20-Valor do titulo invalido';
         21: Result := '21-Especie do titulo invalida';
         22: Result := '22-Especie nao permitida para a carteira';
         24: Result := '24-Data de emissao invalida';
         38: Result := '38-Prazo para protesto invalido';
         44: Result := '44-Agencia cedente nao prevista';
         45: Result := '45-Nome Sacado nao informado';
         46: Result := '46-Tipo/numero inscricao sacado invalido';
         47: Result := '47-Endereco sacado nao informado';
         48: Result := '48-CEP invalido';
         63: Result := '63-Entrada para titulo ja cadastrado';
      else
         Result := IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
    toRetornoLiquidado:
      case CodMotivo of
         00: Result := '00-Titulo pago com dinheiro';
         15: Result := '15-Titulo pago com cheque';
         42: Result := '42-Rateio nao efetuado';
      else
         Result := IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
    toRetornoBaixadoViaArquivo:
      case CodMotivo of
         00: Result := '00-Ocorrencia aceita';
         10: Result := '10=Baixa comandada pelo cliente';
      else
         Result := IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
    toRetornoBaixadoInstAgencia:
         case CodMotivo of
            00: Result := '00-Baixado comandada';
         else
            Result := IntToStrZero(CodMotivo,2) +' - Outros Motivos';
         end;
    toRetornoLiquidadoEmCartorio:
      case CodMotivo of
         00: Result := '00-Pago com dinheiro';
         15: Result := '15-Pago com cheque';
      else
         Result := IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
    toRetornoEntradaRejeitaCEPIrregular:
      case CodMotivo of
         00: Result := '00-CEP invalido';
      else
         Result := IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
    toRetornoBaixaRejeitada:
      case CodMotivo of
         04: Result := '04-Codigo de ocorrencia nao permitido para a carteira';
         07: Result := '07-Agencia\Conta\Digito invalidos';
         08: Result := '08-Nosso numero invalido';
         10: Result := '10-Carteira invalida';
         15: Result := '15-Carteira\Agencia\Conta\NossoNumero invalidos';
         40: Result := '40-Titulo com ordem de protesto emitido';
         60: Result := '60-Movimento para titulo nao cadastrado';
         77: Result := '70-Transferencia para desconto nao permitido para a carteira';
         85: Result := '85-Titulo com pagamento vinculado';
      else
         Result := IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
    toRetornoALteracaoOutrosDadosRejeitada:
      case CodMotivo of
         01: Result := '01-Código do Banco inválido';
         04: Result := '04-Código de ocorrência não permitido para a carteira';
         05: Result := '05-Código da ocorrência não numérico';
         08: Result := '08-Nosso número inválido';
         15: Result := '15-Característica da cobrança incompatível';
         16: Result := '16-Data de vencimento inválido';
         17: Result := '17-Data de vencimento anterior a data de emissão';
         18: Result := '18-Vencimento fora do prazo de operação';
         24: Result := '24-Data de emissão Inválida';
         29: Result := '29-Valor do desconto maior/igual ao valor do Título';
         30: Result := '30-Desconto a conceder não confere';
         31: Result := '31-Concessão de desconto já existente ( Desconto anterior )';
         33: Result := '33-Valor do abatimento inválido';
         34: Result := '34-Valor do abatimento maior/igual ao valor do Título';
         38: Result := '38-Prazo para protesto inválido';
         39: Result := '39-Pedido de protesto não permitido para o Título';
         40: Result := '40-Título com ordem de protesto emitido';
         42: Result := '42-Código para baixa/devolução inválido';
         60: Result := '60-Movimento para Título não cadastrado';
         85: Result := '85-Título com Pagamento Vinculado.';
      else
         Result := IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
    toRetornoInstrucaoRejeitada:
      case CodMotivo of
         01 : Result := '01-Código do Banco inválido';
         02 : Result := '02-Código do registro detalhe inválido';
         04 : Result := '04-Código de ocorrência não permitido para a carteira';
         05 : Result := '05-Código de ocorrência não numérico';
         07 : Result := '07-Agência/Conta/dígito inválidos';
         08 : Result := '08-Nosso número inválido';
         10 : Result := '10-Carteira inválida';
         15 : Result := '15-Características da cobrança incompatíveis';
         16 : Result := '16-Data de vencimento inválida';
         17 : Result := '17-Data de vencimento anterior a data de emissão';
         18 : Result := '18-Vencimento fora do prazo de operação';
         20 : Result := '20-Valor do título inválido';
         21 : Result := '21-Espécie do Título inválida';
         22 : Result := '22-Espécie não permitida para a carteira';
         24 : Result := '24-Data de emissão inválida';
         28 : Result := '28-Código de desconto inválido';
         29 : Result := '29-Valor do desconto maior/igual ao valor do Título';
         30 : Result := '30-Desconto a conceder não confere';
         31 : Result := '31-Concessão de desconto - Já existe desconto anterior';
         33 : Result := '33-Valor do abatimento inválido';
         34 : Result := '34-Valor do abatimento maior/igual ao valor do Título';
         36 : Result := '36-Concessão abatimento - Já existe abatimento anterior';
         38 : Result := '38-Prazo para protesto inválido';
         39 : Result := '39-Pedido de protesto não permitido para o Título';
         40 : Result := '40-Título com ordem de protesto emitido';
         41 : Result := '41-Pedido cancelamento/sustação para Título sem instrução de protesto';
         42 : Result := '42-Código para baixa/devolução inválido';
         45 : Result := '45-Nome do Sacado não informado';
         46 : Result := '46-Tipo/número de inscrição do Sacado inválidos';
         47 : Result := '47-Endereço do Sacado não informado';
         48 : Result := '48-CEP Inválido';
         60 : Result := '60-Movimento para Título não cadastrado';
         85 : Result := '85-Título com pagamento vinculado';
         86 : Result := '86-Seu número inválido';
      else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
    toRetornoDesagendamentoDebitoAutomatico:
         Result := IntToStrZero(CodMotivo,2) + ' - Outros Motivos';
   else
      Result := IntToStrZero(CodMotivo,2) + ' - Outros Motivos';
   end;
end;

function TACBrBancoUnicredRS.CodOcorrenciaToTipoRemessa(const CodOcorrencia:Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02 : Result:= toRemessaBaixar;                          {Pedido de Baixa}
    03 : Result:= toRemessaProtestoFinsFalimentares;        {Pedido de Protesto Falimentar}
    04 : Result:= toRemessaConcederAbatimento;              {Concessão de Abatimento}
    05 : Result:= toRemessaCancelarAbatimento;              {Cancelamento de Abatimento concedido}
    06 : Result:= toRemessaAlterarVencimento;               {Alteração de vencimento}
    07 : Result:= toRemessaAlterarControleParticipante;     {Alteração do controle do participante}
    08 : Result:= toRemessaAlterarNumeroControle;           {Alteração de seu número}
    09 : Result:= toRemessaProtestar;                       {Pedido de protesto}
    18 : Result:= toRemessaCancelarInstrucaoProtestoBaixa;  {Sustar protesto e baixar}
    19 : Result:= toRemessaCancelarInstrucaoProtesto;       {Sustar protesto e manter na carteira}
    22 : Result:= toRemessaTransfCessaoCreditoIDProd10;     {Transferência Cessão crédito ID. Prod.10}
    23 : Result:= toRemessaTransferenciaCarteira;           {Transferência entre Carteiras}
    24 : Result:= toRemessaDevTransferenciaCarteira;        {Dev. Transferência entre Carteiras}
    31 : Result:= toRemessaOutrasOcorrencias;               {Alteração de Outros Dados}
    68 : Result:= toRemessaAcertarRateioCredito;            {Acerto nos dados do rateio de Crédito}
    69 : Result:= toRemessaCancelarRateioCredito;           {Cancelamento do rateio de crédito.}
  else
     Result:= toRemessaRegistrar;                           {Remessa}
  end;
end;


end.


