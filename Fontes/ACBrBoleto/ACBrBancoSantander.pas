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

unit ACBrBancoSantander;

interface

uses
  Classes, SysUtils,
  ACBrBoleto;

type

  { TACBrBancoSantander }

  TACBrBancoSantander = class(TACBrBancoClass)
  private
  protected
    vTotalTitulos : Double;
  public
    Constructor create(AOwner: TACBrBanco);
    function CalcularDigitoVerificador(const ACBrTitulo:TACBrTitulo): String; override;
    function MontarCodigoBarras(const ACBrTitulo : TACBrTitulo): String; override;
    function MontarCampoNossoNumero(const ACBrTitulo :TACBrTitulo): String; override;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String; override;
    function GerarRegistroHeader240(NumeroRemessa: Integer): String; override;
    function GerarRegistroTransacao240(ACBrTitulo : TACBrTitulo): String; override;
    function GerarRegistroTrailler240(ARemessa : TStringList): String;  override;
    procedure GerarRegistroHeader400(NumeroRemessa : Integer; aRemessa: TStringList); override;
    procedure GerarRegistroTransacao400(ACBrTitulo : TACBrTitulo; aRemessa: TStringList); override;
    procedure GerarRegistroTrailler400(ARemessa:TStringList);  override;
    Procedure LerRetorno240(ARetorno:TStringList); override;
    Procedure LerRetorno400(ARetorno:TStringList); override;

    function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia) : String; override;
    function CodOcorrenciaToTipo(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
    function TipoOCorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia):String; override;
    function CodMotivoRejeicaoToDescricao(const TipoOcorrencia:TACBrTipoOcorrencia; CodMotivo:Integer): String; override;
  end;

implementation

uses
  {$IFDEF COMPILER6_UP} dateutils {$ELSE} ACBrD5 {$ENDIF},
  StrUtils, math,
  ACBrUtil;

{ TACBrBancoSantander }

constructor TACBrBancoSantander.create(AOwner: TACBrBanco);
begin
   inherited create(AOwner);
   fpDigito := 7;
   fpNome   := 'Santander';
   fpNumero:= 033;
   fpTamanhoMaximoNossoNum := 12;
   fpTamanhoCarteira:= 3;
   fpTamanhoConta := 11;
end;

function TACBrBancoSantander.CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo ): String;
begin
   Modulo.CalculoPadrao;
   Modulo.MultiplicadorFinal := 9;
   Modulo.Documento := ACBrTitulo.NossoNumero;
   Modulo.Calcular;

   Result:= IntToStr(Modulo.DigitoFinal);
end;

function TACBrBancoSantander.MontarCodigoBarras ( const ACBrTitulo: TACBrTitulo) : String;
var
  CodigoBarras, FatorVencimento, DigitoCodBarras, DigitoNossoNumero:String;
begin

   with ACBrTitulo.ACBrBoleto do
   begin
      DigitoNossoNumero := CalcularDigitoVerificador(ACBrTitulo);
      FatorVencimento   := CalcularFatorVencimento(ACBrTitulo.Vencimento);

      CodigoBarras := '033'+'9'+ FatorVencimento +
                       IntToStrZero(Round(ACBrTitulo.ValorDocumento*100),10) +
                       '9'+ PadLeft(trim(Cedente.CodigoCedente),7,'0') +
                       PadLeft(ACBrTitulo.NossoNumero + DigitoNossoNumero, 13,'0') +
                       '0'+ PadLeft(trim(Cedente.Modalidade),3,'0');



      DigitoCodBarras := CalcularDigitoCodigoBarras(CodigoBarras);
   end;

   Result:= '033' + '9'+ DigitoCodBarras + Copy(CodigoBarras,5,39);
end;

function TACBrBancoSantander.MontarCampoNossoNumero (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
   with ACBrTitulo do
   begin
      case StrToIntDef(Carteira,0) of
         5: Carteira := '101';
         6: Carteira := '201';
         4: Carteira := '102';
      end;
   end;

   Result:= PadLeft(ACBrTitulo.NossoNumero,12,'0')+ ' '+ CalcularDigitoVerificador(ACBrTitulo);
end;

function TACBrBancoSantander.MontarCampoCodigoCedente (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
   Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia+'-'+
             ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito+'/'+
             ACBrTitulo.ACBrBoleto.Cedente.CodigoCedente
end;

function TACBrBancoSantander.GerarRegistroHeader240(NumeroRemessa: Integer): String;
begin
// by Jéter Rabelo Ferreira - 06/2014
   with ACBrBanco.ACBrBoleto.Cedente do
   begin
      { REGISTRO HEADER DO ARQUIVO REMESSA }
      Result := '033'                                      + // 001 - 003 / Código do Banco na compensação
                '0000'                                     + // 004 - 007 / Lote de serviço
                '0'                                        + // 008 - 008 / Tipo de registro
                Space(8)                                   + // 009 - 016 / Reservado (uso Banco)
                ifthen(TipoInscricao = pFisica, '1', '2')  + // 017 - 017 / Tipo de inscrição da empresa
                PadLeft(trim(OnlyNumber(CNPJCPF)),15,'0')     + // 018 - 032 / Nº de inscrição da empresa
                PadLeft(CodigoTransmissao, 15)                + // 033 - 047 / Código de Transmissão
                Space(25)                                  + // 048 - 072 / Reservado (uso Banco)
                PadRight(Nome, 30)                             + // 073 - 102 / Nome da Empresa
                PadRight('BANCO SANTANDER', 30)                + // 103 - 132 / Nome do Banco(BANCO SANTANDER)
                Space(10)                                  + // 133 - 142 / Reservado (uso Banco)
                '1'                                        + // 143 - 143 / Código remessa = 1
                FormatDateTime('ddmmyyyy',Now)             + // 144 - 151 / Data de geração do arquivo
                Space(6)                                   + // 152 - 157 / Reservado (uso Banco)
                PadLeft(IntToStr(NumeroRemessa), 6, '0')      + // 158 - 163 / Nº seqüencial do arquivo
                '040'                                      + // 164 - 166 / Nº da versão do layout do arquivo
                Space(74)                                  ; // 167 - 240 / Reservado (uso Banco)

      { REGISTRO HEADER DO LOTE REMESSA }
      Result := Result + #13#10 +
                '033'                                      + // 001 - 003 / Código do Banco na compensação
                '0001'                                     + // 004 - 007 / Numero do lote remessa
                '1'                                        + // 008 - 008 / Tipo de registro
                'R'                                        + // 009 - 009 / Tipo de operação
                '01'                                       + // 010 - 011 / Tipo de serviço
                Space(2)                                   + // 012 - 013 / Reservado (uso Banco)
                '030'                                      + // 014 - 016 / Nº da versão do layout do lote
                Space(1)                                   + // 017 - 017 / Reservado (uso Banco)
                ifthen(TipoInscricao = pFisica, '1', '2')  + // 018 - 018 / Tipo de inscrição da empresa
                PadLeft(trim(OnlyNumber(CNPJCPF)),15,'0')     + // 019 - 033 / Nº de inscrição da empresa
                Space(20)                                  + // 034 - 053 / Reservado (uso Banco)
                PadLeft(CodigoTransmissao, 15)                + // 054 - 068 / Código de Transmissão
                Space(5)                                   + // 069 - 073 / Reservado (uso Banco)
                PadRight(Nome, 30)                             + // 074 - 0103 / Nome do Cedente
                Space(40)                                  + // 104 - 143 / Mensagem 1
                Space(40)                                  + // 144 - 183 / Mensagem 2
                PadLeft(IntToStr(NumeroRemessa), 8, '0')      + // 184 - 191 / Nº temessa
                FormatDateTime('ddmmyyyy',Now)             + // 192 - 199 / Data de geração do arquivo
                Space(41)                                  ; // 200 - 240 / Reservado (uso Banco)

      Result := UpperCase(Result);
   end;
end;

procedure TACBrBancoSantander.GerarRegistroHeader400(NumeroRemessa : Integer; aRemessa: TStringList);
var
  wLinha: String;
begin
   vTotalTitulos:= 0;
   with ACBrBanco.ACBrBoleto.Cedente do
   begin
      wLinha:= '0'                                        + // ID do Registro
               '1'                                        + // ID do Arquivo( 1 - Remessa)
               'REMESSA'                                  + // Literal de Remessa
               '01'                                       + // Código do Tipo de Serviço
               PadRight( 'COBRANCA', 15 )                     + // Descrição do tipo de serviço
               PadLeft( CodigoTransmissao, 20, '0')          + // Codigo da Empresa no Banco
               PadRight( Nome, 30)                            + // Nome da Empresa
               '033'+ PadRight('SANTANDER', 15)               + // Código e Nome do Banco(237 - Bradesco)
               FormatDateTime('ddmmyy',Now)               + // Data de geração do arquivo + brancos
               StringOfChar( '0', 16)                     +
               Space(275)+ '000'                          + // Nr. Sequencial de Remessa + brancos
               IntToStrZero(1,6);                           // Nr. Sequencial de Remessa + brancos + Contador

      aRemessa.Text:= aRemessa.Text + UpperCase(wLinha);
   end;
end;

function TACBrBancoSantander.GerarRegistroTransacao240(ACBrTitulo: TACBrTitulo): String;
var
  ISequencia: Integer;
  iCarteira: Integer;
  sCodMovimento, sAgencia, sCCorrente: string;
  sDigitoNossoNumero, sTipoCobranca, sTipoDocto, sTipoCarteira: string;
  sEspecie, sDataMoraJuros, sDataDesconto: string;
  STipoJuros, sTipoDesconto, sDiasProtesto: string;
  sTipoInscricao, sEndereco, sMensagem: string;
  aTipoInscricao: Char;
  function MontarInstrucoes1: string;
  begin
    with ACBrTitulo do
    begin
      if Mensagem.Count = 0 then
      begin
        Result := PadRight('', 80, ' '); // 2 registros
        Exit;
      end;

      Result := '';
      if Mensagem.Count >= 1 then
      begin
        Result := Result +
                  Copy(PadRight(Mensagem[0], 40, ' '), 1, 40);
      end;

      if Mensagem.Count >= 2 then
      begin
        Result := Result +
                  Copy(PadRight(Mensagem[1], 40, ' '), 1, 40)
      end
      else
      begin
        if (Result <> EmptyStr) then
          Result := Result + PadRight('', 40, ' ')  // 1 registro
        else
          Result := Result + PadRight('', 80, ' '); // 2 registros
        Exit;
      end;
    end;
  end;

  function MontarInstrucoes2: string;
  begin
    with ACBrTitulo do
    begin
      if Mensagem.Count <= 2 then
      begin
        // Somente duas linhas, foi montado o MonarInstrucoes1
        Result := PadRight('', 200, ' '); // 5 registros
        Exit;
      end;

      Result := '';
      if Mensagem.Count >= 3 then
      begin
        Result := Copy(PadRight(Mensagem[2], 40, ' '), 1, 40);
      end;

      if Mensagem.Count >= 4 then
      begin
        Result := Result +
                  Copy(PadRight(Mensagem[3], 40, ' '), 1, 40)
      end;

      if Mensagem.Count >= 5 then
      begin
        Result := Result +
                  Copy(PadRight(Mensagem[4], 40, ' '), 1, 40)
      end;

      if Mensagem.Count >= 6 then
      begin
        Result := Result +
                  Copy(PadRight(Mensagem[5], 40, ' '), 1, 40)
      end;

      if Mensagem.Count >= 7 then
      begin
        Result := Result +
                  Copy(PadRight(Mensagem[6], 40, ' '), 1, 40)
      end;

      // Acertar a quantidade de caracteres
      Result := PadRight(Result, 200);
    end;
  end;

begin
// by Jéter Rabelo Ferreira - 06/2014
  with ACBrTitulo do
  begin
    case OcorrenciaOriginal.Tipo of
       toRemessaBaixar                        : sCodMovimento := '02'; {Pedido de Baixa}
       toRemessaConcederAbatimento            : sCodMovimento := '04'; {Concessão de Abatimento}
       toRemessaCancelarAbatimento            : sCodMovimento := '05'; {Cancelamento de Abatimento concedido}
       toRemessaAlterarVencimento             : sCodMovimento := '06'; {Alteração de vencimento}
       toRemessaAlterarControleParticipante   : sCodMovimento := '07'; {Alteração Número Controle Cedente}
       toRemessaAlterarNumeroControle         : sCodMovimento := '08'; {Alteração de seu número}
       toRemessaProtestar                     : sCodMovimento := '09'; {Pedido de protesto}
       toRemessaCancelarInstrucaoProtesto     : sCodMovimento := '18'; {Sustar protesto e manter na carteira}
       toRemessaConcederDesconto              : sCodMovimento := '10'; {Concessão de Desconto}
       toRemessaCancelarDesconto              : sCodMovimento := '11'; {Cancelamento de Desconto}
       toRemessaNaoProtestar                  : sCodMovimento := '98'; {Não Protestar (Antes de iniciar o ciclo de protesto )}
    else
       sCodMovimento := '01';                                          {Remessa}
    end;

    iCarteira := StrToIntDef(ACBrTitulo.Carteira, 0 );

    sAgencia := PadLeft(OnlyNumber(ACBrTitulo.ACBrBoleto.Cedente.Agencia) +
                        ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito,5,'0');

    // Tamanho da conta corrente definida como padrão de 11 digitos, porém no arquivo
    // remessa a conta solicitada é de 8 dígitos.
    // Devemos retirar os zeros a esquerda da conta

    sCCorrente := OnlyNumber(ACBrTitulo.ACBrBoleto.Cedente.Conta);
    sCCorrente := Copy(SCCorrente, Length(sCCorrente) - 8, 9) +
                  OnlyNumber(ACBrTitulo.ACBrBoleto.Cedente.ContaDigito);

    sDigitoNossoNumero := CalcularDigitoVerificador(ACBrTitulo);

    case CaracTitulo of
      tcSimples     : sTipoCobranca  := '1'; {Cobrança Simples (Sem Registro e Eletrônica com Registro)}
      tcCaucionada  : sTipoCobranca  := '3'; {Cobrança Caucionada (Eletrônica com Registro e Convencional com Registro)}
      tcDescontada  : sTipoCobranca  := '4'; {Cobrança Descontada (Eletrônica com Registro)}
      tcVinculada   : sTipoCobranca  := '5'; {Cobrança Simples (Rápida com Registro)}
      { TODO :
          6 = Cobrança Caucionada (Rápida com Registro)
          8 = Cobranca Cessao (Eletronica com Registro)
      }
    end;

    case ACBrBoleto.Cedente.TipoCarteira of
      tctSimples: sTipoCarteira := '2';
      tctRegistrada: sTipoCarteira := '1';
      else 
       sTipoCarteira := '2';
    end;

    case ACBrBoleto.Cedente.TipoDocumento of
      Tradicional: sTipoDocto := '1';
      Escritural: sTipoDocto := '2';
    end;

    if sTipoDocto = '' then
      sTipoDocto := '1'; // Tradicional

    if Trim(EspecieDoc) = 'DM' then      {DM - DUPLICATA MERCANTIL}
      sEspecie := '02'
    else if Trim(EspecieDoc) = 'DS' then {DS - DUPLICATA DE SERVICO}
      sEspecie := '04'
    else if Trim(EspecieDoc) = 'NP' then {NP - NOTA PROMISSORIA}
      sEspecie := '12'
    else if Trim(EspecieDoc) = 'NR' then {NR - NOTA PROMISSORIA RURAL}
      sEspecie := '13'
    else if Trim(EspecieDoc) = 'RC' then {RC - RECIBO}
      sEspecie := '17'
    else if Trim(EspecieDoc) = 'AP' then {AP – APOLICE DE SEGURO}
      sEspecie := '20'
    else if Trim(EspecieDoc) = 'CH' then {CH - CHEQUE}
      sEspecie := '97'
    else if Trim(EspecieDoc) = 'CH' then {ND - NOTA PROMISSORIA DIRETA}
      sEspecie := '98'
    else
    begin
      if not MatchText(EspecieDoc, ['02', '04', '12', '13', '17', '20', '97', '98']) then
        raise Exception.Create('Espécie de documento informada incorretamente!');

      sEspecie := EspecieDoc;
    end;

    if (ValorMoraJuros > 0) then
    begin
      STipoJuros := '1';  // Valor por dia
//      STipoJuros := '2';  // Taxa Mensal
      if DataMoraJuros <> 0 then
        sDataMoraJuros := FormatDateTime('ddmmyyyy', DataMoraJuros)
      else
        sDataMoraJuros := PadLeft('', 8, '0');
    end
    else
    begin
      sDataMoraJuros := PadLeft('', 8, '0');
      STipoJuros := '3'; // Isento
    end;

    if ValorDesconto > 0 then
    begin
      sTipoDesconto := '1'; // Valor fixo ate a data informada – Informar o valor no campo “valor de desconto a ser concedido”.
      if DataDesconto <> 0 then
      begin
        sDataDesconto := FormatDateTime('ddmmyyyy', DataDesconto);
        sTipoDesconto := '2';
      end
      else
      begin
        sTipoDesconto := '0'; // ISENTO
        sDataDesconto := PadLeft('', 8, '0');
      end;
    end
    else
    begin
      sTipoDesconto := '0'; // ISENTO
      sDataDesconto := PadLeft('', 8, '0');
    end;

    {Instruções}
    if (DataProtesto <> 0) and
       (DataProtesto > Vencimento) then
    begin
      if (Trim(Instrucao2) = '') then
        Instrucao2 := '1' // Protestar Dias Corridos
      else
      begin
        if not MatchText(Instrucao2, ['0', '1', '2', '3', '9']) then
          raise Exception.Create('Código de protesto informado incorretamente!');
      end;
      // Calcular os dias para protesto
      sDiasProtesto := PadLeft(IntToStr(Trunc(DataProtesto) - Trunc(Vencimento)), 2, '0');
    end
    else
    begin
      Instrucao1 := '0';  // Não protestar
      SDiasProtesto := '00';
    end;

    // Baixa/Devolução
    if Instrucao2 = '' then
      Instrucao2 := '2' // NAO BAIXAR / NAO DEVOLVER
    else
    begin
      if not MatchText(Instrucao2, ['1', '2', '3']) then
        raise Exception.Create('Código de Baixa/Devolução informado incorretamente!');
    end;

    case Sacado.Pessoa of
       pFisica  : sTipoInscricao := '1';
       pJuridica: sTipoInscricao := '2';
       pOutras  : sTipoInscricao := '9';
    end;

    if Sacado.SacadoAvalista.CNPJCPF <> '' then
     begin
      case Sacado.SacadoAvalista.Pessoa of
        pFisica  : aTipoInscricao := '1';
        pJuridica: aTipoInscricao := '2';
        pOutras  : aTipoInscricao := '9';
      end;
     end
    else
      aTipoInscricao:= '0';


    sEndereco := PadRight(Sacado.Logradouro + ' ' +
                      Sacado.Numero + ' ' +
                      Sacado.Complemento , 40, ' ');

    ISequencia := (ACBrBoleto.ListadeBoletos.IndexOf(ACBrTitulo) * 4) + 1;
    {SEGMENTO P}
    Result := '033'                                            + // 001 - 003 / Código do Banco na compensação
              '0001'                                           + // 004 - 007 / Numero do lote remessa
              '3'                                              + // 008 - 008 / Tipo de registro
              IntToStrZero(ISequencia ,5)                      + // 009 - 013 / Número seqüencial do registro no lote
              'P'                                              + // 014 - 014 / Cód. Segmento do registro detalhe
              Space(1)                                         + // 015 - 015 / Reservado (uso Banco)
              sCodMovimento                                    + // 016 - 017 / Código de movimento remessa
              Copy(sAgencia, 1, 4)                             + // 018 – 021 / Agência do Cedente
              Copy(sAgencia, 5, 1)                             + // 022 – 022 / Dígito da Agência do Cedente
              Copy(sCCorrente, 1, 9)                           + // 023 - 031 / da conta corrente
              Copy(sCCorrente, 10, 1)                          + // 032 – 032 / Dígito verificador da conta
              Copy(sCCorrente, 1, 9)                           + // 033 - 041 / Conta cobrança
              Copy(sCCorrente, 10, 1)                          + // 042 - 042 / Dígito da conta cobrança
              Space(2)                                         + // 043 - 044 / Reservado (uso Banco)
              NossoNumero + sDigitoNossoNumero                 + // 045 – 057 / Identificação do título no Banco (Nosso Número
              sTipoCobranca                                    + // 058 - 058 / Tipo de cobrança
              sTipoCarteira                                    + // 059 - 059 / Forma de Cadastramento = 1 Registrada / 2 Sem Registro
              sTipoDocto                                       + // 060 - 060 / Tipo de documento
              Space(1)                                         + // 061 - 061 / Reservado (uso Banco)
              Space(1)                                         + // 062 - 062 / Reservado (uso Banco)
              PadRight(Copy(SeuNumero, 1, 15), 15)                 + // 063 - 077 / Nº do documento
              FormatDateTime('ddmmyyyy',Vencimento)            + // 078 - 085 / Data de vencimento do título
              IntToStrZero(round(ValorDocumento * 100), 15)    + // 086 - 100 / Valor nominal do título
              PadLeft('0', 4, '0')                                + // 101 - 104 / Agência encarregada da cobrança
              '0'                                              + // 105 - 105 / Dígito da Agência encarregada da cobrança
              Space(1)                                         + // 106 - 106 / Reservado (uso Banco)
              sEspecie                                         + // 107 – 108 / Espécie do título
              ifThen(Aceite = atSim,  'S', 'N')                + // 109 - 109 / Identif. de título Aceito/Não Aceito
              FormatDateTime('ddmmyyyy',DataDocumento)         + // 110 - 117 / Data da emissão do título
              STipoJuros                                       + // 118 - 118 / Código do juros de mora
              sDataMoraJuros                                   + // 119 - 126 / Data do juros de mora
              IntToStrZero(round(ValorMoraJuros * 100), 15)    + // 127 - 141 / Valor da mora/dia ou Taxa mensal
              sTipoDesconto                                    + // 142 - 142 / Código do desconto 1
              sDataDesconto                                    + // 143 - 150 / Data de desconto 1
              IntToStrZero(round(ValorDesconto * 100), 15)     + // 151 - 165 / Valor ou Percentual do desconto concedido
              IntToStrZero(round(ValorIOF * 100), 15)          + // 166 - 180 / Valor do IOF a ser recolhido
              IntToStrZero(round(ValorAbatimento * 100), 15)   + // 181 - 195 / Valor do abatimento
              PadRight(NossoNumero, 25)                            + // 196 - 220 / Identificação do título na empresa
              Instrucao1                                       + // 221 - 221 / Código para protesto
              sDiasProtesto                                    + // 222 - 223 / Número de dias para protesto
              Instrucao2                                       + // 224 - 224 / Código para Baixa/Devolução
              '0'                                              + // 225 - 225 / Reservado (uso Banco)
              '00'                                             + // 226 - 227 / Número de dias para Baixa/Devolução
              '00'                                             + // 228 - 229 / Código da moeda
              Space(11)                                        ; // 230 – 240 / Reservado (uso Banco)
    {SEGMENTO P - FIM}

    Inc(ISequencia);
    {SEGMENTO Q}
    Result := Result + #13#10 +
              '033'                                            + // 001 - 003 / Código do Banco na compensação
              '0001'                                           + // 004 - 007 / Numero do lote remessa
              '3'                                              + // 008 - 008 / Tipo de registro
              IntToStrZero(ISequencia ,5)                      + // 009 - 013 / Número seqüencial do registro no lote
              'Q'                                              + // 014 - 014 / Cód. Segmento do registro detalhe
              Space(1)                                         + // 015 - 015 / Reservado (uso Banco)
              sCodMovimento                                    + // 016 - 017 / Código de movimento remessa
              sTipoInscricao                                   + // 018 - 018 / Tipo de inscrição do sacado
              PadLeft(trim(OnlyNumber(Sacado.CNPJCPF)),15,'0')    + // 019 - 033 / Número de inscrição do sacado
              PadRight(Trim(Sacado.NomeSacado), 40)                + // 034 - 073 / Nome sacado
              sEndereco                                        + // 074 - 113 / Endereço sacado
              PadRight(Trim(Sacado.Bairro), 15)                    + // 114 - 128 / Bairro sacado
              PadLeft(Copy(OnlyNumber(Sacado.CEP), 1, 5), 5, '0') + // 129 - 133 / Cep sacado
              PadLeft(Copy(OnlyNumber(Sacado.CEP), 6, 3), 3, '0') + // 134 - 136 / Sufixo do Cep do sacado
              PadRight(Trim(Sacado.Cidade), 15)                    + // 137 - 151 / Cidade do sacado
              Sacado.UF                                        + // 152 - 153 / Unidade da federação do sacado
              aTipoInscricao                                   + // 154 - 154 / Tipo de inscrição sacador/avalista
              PadLeft(Sacado.SacadoAvalista.CNPJCPF, 15,'0')       + // 155 - 169 / Nº de inscrição sacador/avalista
              PadRight(Sacado.SacadoAvalista.NomeAvalista,40,' ')  + // 170 - 209 / Nome do sacador/avalista
              '000'                                            + // 210 – 212 / Identificador de carne
              '000'                                            + // 213 – 215 / Seqüencial da Parcela ou número inicial da parcela
              '000'                                            + // 216 – 218 / Quantidade total de parcelas
              '000'                                            + // 219 – 221 / Número do plano
              Space(19)                                        ; // 230 – 240 / Reservado (uso Banco)
    {SEGMENTO Q - FIM}

    Inc(ISequencia);
    {SEGMENTO R}
    Result := Result + #13#10 +
              '033'                                                      + // 001 - 003 / Código do Banco na compensação
              '0001'                                                     + // 004 - 007 / Numero do lote remessa
              '3'                                                        + // 008 - 008 / Tipo de registro
              IntToStrZero(ISequencia ,5)                                + // 009 - 013 / Número seqüencial do registro no lote
              'R'                                                        + // 014 - 014 / Cód. Segmento do registro detalhe
              Space(1)                                                   + // 015 - 015 / Reservado (uso Banco)
              sCodMovimento                                              + // 016 - 017 / Código de movimento remessa
              '0'                                                        + // 018 - 018 / Código do desconto 2
              PadLeft('', 8, '0')                                           + // 019 - 026 / Data do desconto 2
              IntToStrZero(0, 15)                                        + // 027 - 041 / Valor/Percentual a ser concedido
              Space(24)                                                  + // 042 – 065 / Reservado (uso Banco)
              '1'                                                        + // 066 - 066 / Código da multa
              PadLeft('', 8, '0')                                           + // 067 - 074 / Data da multa
              IntToStrZero(round(ValorDocumento * PercentualMulta), 15)  + // 075 - 089 / Valor/Percentual a ser aplicado
              Space(10)                                                  + // 090 - 099 / Reservado (uso Banco)
              MontarInstrucoes1                                          + // 100 - 139 / Mensagem 3
                                                                           // 140 - 179 / Mensagem 4
              Space(61)                                                  ; // 180 - 240 / Reservado (uso Banco)
    {SEGMENTO R - FIM}

    Inc(ISequencia);
    {SEGMENTO S}
    // Existe um Formmulário 1 - Especial, que não será implementado
    // Será implementado do Formulário 2
    Result := Result + #13#10 +
              '033'                                            + // 001 - 003 / Código do Banco na compensação
              '0001'                                           + // 004 - 007 / Numero do lote remessa
              '3'                                              + // 008 - 008 / Tipo de registro
              IntToStrZero(ISequencia ,5)                      + // 009 - 013 / Número seqüencial do registro no lote
              'S'                                              + // 014 - 014 / Cód. Segmento do registro detalhe
              Space(1)                                         + // 015 - 015 / Reservado (uso Banco)
              sCodMovimento                                    + // 016 - 017 / Código de movimento remessa
              '2'                                              + // 018 - 018 / Identificação da impressão
              MontarInstrucoes2                                + // 019 - 058 / Mensagem 5
                                                                 // 059 - 098 / Mensagem 6
                                                                 // 099 - 138 / Mensagem 7
                                                                 // 139 - 178 / Mensagem 8
                                                                 // 179 - 218 / Mensagem 9
              Space(22)                                        ; // 219 - 240 / Reservado (uso Banco)
    {SEGMENTO S - FIM}
  end;
end;

procedure TACBrBancoSantander.GerarRegistroTransacao400(ACBrTitulo :TACBrTitulo; aRemessa: TStringList);
var
  DigitoNossoNumero, Ocorrencia,aEspecie :String;
  Protesto, aAgencia, TipoSacado, wLinha :String;
  aCarteira, I: Integer;
begin

   aCarteira := StrToIntDef(ACBrTitulo.Carteira, 0 );

   if aCarteira = 101  then
      aCarteira:= 5
   else if aCarteira = 201 then
      aCarteira:= 6
   else if aCarteira = 102 then
      aCarteira:= 4;

   if aCarteira = 5 then
      aAgencia := PadLeft(OnlyNumber(ACBrTitulo.ACBrBoleto.Cedente.Agencia) +
                       ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito,5,'0')
   else
      aAgencia:= '00000';

   vTotalTitulos:= vTotalTitulos+ ACBrTitulo.ValorDocumento;
   with ACBrTitulo do
   begin
      DigitoNossoNumero := CalcularDigitoVerificador(ACBrTitulo);

      {Pegando Código da Ocorrencia}
      case OcorrenciaOriginal.Tipo of
         toRemessaBaixar                        : Ocorrencia := '02'; {Pedido de Baixa}
         toRemessaConcederAbatimento            : Ocorrencia := '04'; {Concessão de Abatimento}
         toRemessaCancelarAbatimento            : Ocorrencia := '05'; {Cancelamento de Abatimento concedido}
         toRemessaAlterarVencimento             : Ocorrencia := '06'; {Alteração de vencimento}
         toRemessaAlterarControleParticipante   : Ocorrencia := '07'; {Alteração Número Controle Cedente}
         toRemessaAlterarNumeroControle         : Ocorrencia := '08'; {Alteração de seu número}
         toRemessaProtestar                     : Ocorrencia := '09'; {Pedido de protesto}
         toRemessaCancelarInstrucaoProtesto     : Ocorrencia := '18'; {Sustar protesto e manter na carteira}
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
      else if trim(EspecieDoc) = 'RC' then
         aEspecie:= '05'
      else if trim(EspecieDoc) = 'DS' then
         aEspecie:= '06'
      else if trim(EspecieDoc) = 'LC' then
         aEspecie:= '07'
      else
         aEspecie := EspecieDoc;

      {Pegando campo Intruções}
      if (DataProtesto > 0) and (DataProtesto > Vencimento) then //and (Instrucao1 = '06') then
       begin
         Protesto :=  IntToStrZero(DaysBetween(DataProtesto,Vencimento),2);
         if (trim(Instrucao1) <> '06' )  and (trim(Instrucao2) <> '06' ) then
            If Trim(Instrucao1) = '' then
               Instrucao1 := '06'
            else
               Instrucao2 := '06';
       end
      else
         Protesto:=  '00';

      {Pegando Tipo de Sacado}
      case Sacado.Pessoa of
         pFisica   : TipoSacado := '01';
         pJuridica : TipoSacado := '02';
      else
         TipoSacado := '99'; //TODO: CHECAR OQ FAZER PARA CEDENTE SEM TIPO
      end;

      with ACBrBoleto do
      begin
         wLinha:= '1'                                                     +  // ID Registro
                  IfThen(Length(Cedente.CNPJCPF) > 12,'02','01')          +
                  PadLeft(trim(OnlyNumber(Cedente.CNPJCPF)),14,'0')          +
                  PadRight(trim(Cedente.CodigoTransmissao),20,'0')            +
                  PadRight( SeuNumero ,25,' ')                                +
                  PadLeft(RightStr(NossoNumero,7),7,'0') + DigitoNossoNumero +
                  IfThen(DataAbatimento < EncodeDate(2000,01,01),
                         '000000',
                         FormatDateTime( 'ddmmyy', DataAbatimento))       +
                  ' '+IfThen(PercentualMulta > 0,'4','0')                 +
                  IntToStrZero( round( PercentualMulta * 100 ), 4)        +
                  '00'+StringOfChar( '0', 13)+space(4)                    +
                  IfThen(DataMoraJuros < EncodeDate(2000,01,01),
                         '000000',
                         FormatDateTime( 'ddmmyy', DataMoraJuros))        +
                   IntToStr(aCarteira) + Ocorrencia                       +
                  PadRight( NumeroDocumento,10,' ')                           +
                  FormatDateTime( 'ddmmyy', Vencimento)                   +
                  IntToStrZero( round( ValorDocumento * 100), 13)         +
                  '033' + aAgencia                                        +
                  PadRight(aEspecie,2) + 'N'                                  +
                  FormatDateTime( 'ddmmyy', DataDocumento )               +
                  PadRight(trim(Instrucao1),2,'0')                            +
                  PadRight(trim(Instrucao2),2,'0')                            +
                  IntToStrZero( round(ValorMoraJuros * 100 ), 13)         +
                  IfThen(DataDesconto < EncodeDate(2000,01,01),
                         '000000',
                         FormatDateTime( 'ddmmyy', DataDesconto))         +
                  IntToStrZero( round( ValorDesconto * 100), 13)          +
                  IntToStrZero( round( ValorIOF * 100 ), 13)              +
                  IntToStrZero( round( ValorAbatimento * 100 ), 13)       +
                  TipoSacado + PadLeft(OnlyNumber(Sacado.CNPJCPF),14,'0')    +
                  PadRight( Sacado.NomeSacado, 40, ' ')                       +
                  PadRight( Sacado.Logradouro + ' '+ Sacado.Numero, 40, ' ')  +
                  PadRight( Sacado.Bairro,12,' ')                             +
                  PadRight( OnlyNumber(Sacado.CEP) , 8, ' ' )                 +
                  PadRight( Sacado.Cidade, 15, ' ') + Sacado.UF               +
				  IfThen(ACBrBoleto.Cedente.TipoInscricao = pJuridica,
                         Space(30),
                         PadRight(Sacado.Avalista, 30, ' ' )
                         )+ ' '+ 'I'                                       +
                  Copy(Cedente.Conta,Length(Cedente.Conta),1)             +
                  Cedente.ContaDigito + Space(6)                          +
                  Protesto + ' '                                          +
                  IntToStrZero( aRemessa.Count + 1, 6 );


         wLinha:= UpperCase(wLinha);

         for I := 0 to Mensagem.count-1 do
            wLinha:= wLinha + #13#10                         +
                     '2' + space(16)                             +
                     PadRight(Cedente.CodigoTransmissao,20,'0')      +
                     Space(10) + '01'                            +
                     PadRight(Mensagem[I],50)                        +
                     Space(283) + 'I'                            +
                     Copy(Cedente.Conta,Length(Cedente.Conta),1) +
                     Cedente.ContaDigito                         +
                     Space(9)                                    +
                     IntToStrZero( aRemessa.Count  + I + 2 , 6 );

         aRemessa.Text:= aRemessa.Text + UpperCase(wLinha);
      end;
   end;
end;

function TACBrBancoSantander.GerarRegistroTrailler240(
  ARemessa: TStringList): String;
begin
// by Jéter Rabelo Ferreira - 06/2014
   {REGISTRO TRAILER DO LOTE}
   Result:= IntToStrZero(ACBrBanco.Numero, 3)                          + // 001 - 003 / Código do Banco na compensação
            '0001'                                                     + // 004 - 007 / Numero do lote remessa
            '5'                                                        + // 008 - 008 / Tipo de registro
            Space(9)                                                   + // 009 - 017 / Reservado (uso Banco)
            IntToStrZero((4 * (ARemessa.Count -1)) + 2, 6)             + // 018 - 023 / Quantidade de registros do lote
            space(217)                                                 ; // 024 - 240 / Reservado (uso Banco)

   {GERAR REGISTRO TRAILER DO ARQUIVO}
   Result:= Result + #13#10 +
            IntToStrZero(ACBrBanco.Numero, 3)                          + // 001 - 003 / Código do Banco na compensação
            '9999'                                                     + // 004 - 007 / Numero do lote remessa
            '9'                                                        + // 008 - 008 / Tipo de registro
            space(9)                                                   + // 009 - 017 / Reservado (uso Banco)
            '000001'                                                   + // 018 - 023 / Quantidade de lotes do arquivo
            IntToStrZero((4 * (ARemessa.Count -1)) + 4, 6)             + // 024 - 029 / Quantidade de registros do arquivo
            space(211)                                                 ; // 030 - 240 / Reservado (uso Banco)
end;

procedure TACBrBancoSantander.GerarRegistroTrailler400( ARemessa:TStringList );
var
  vQtdeLinha : Integer;
  wLinha: String;
begin
   vQtdeLinha := StrToInt(copy(ARemessa.Text,Length(ARemessa.Text)-7,6));//lê a ultima linha gravada para pergar o codigo seq.

   wLinha:= '9'                                            +           // ID Registro
            IntToStrZero( vQtdeLinha + 1, 6 )              +           // Contador de Registros
            IntToStrZero( round( vTotalTitulos* 100), 13)  +           // Valor Total dos Titulos
            StringOfChar( '0', 374)                        +
            IntToStrZero(ARemessa.Count + 1, 6);

   ARemessa.Text:= ARemessa.Text + UpperCase(wLinha);
end;

procedure TACBrBancoSantander.LerRetorno240(ARetorno: TStringList);
var
  Titulo: TACBrTitulo;
  Linha, rCodigoCedente, rCedente, rAgencia, rAgenciaDigito, rConta, rContaDigito, rCNPJCPF : String;
  iLinha : Integer;
  iIdxMotivo: Integer;
  procedure DoVerOcorrencia(AOcorrencia: string);
  begin
    with Titulo.OcorrenciaOriginal do
    begin
      if MatchText(AOcorrencia, ['02', '03', '06', '09', '11', '12', '13', '14'])  then
      begin
        Tipo := CodOcorrenciaToTipo(StrToInt(AOcorrencia));
      end
      else
      begin
        if AOcorrencia = '04' then
          Tipo := toRetornoTransferenciaCarteiraEntrada
        else if AOcorrencia = '05' then
          Tipo := toRetornoTransferenciaCarteiraBaixa
        else if AOcorrencia = '17' then
          Tipo := toRetornoLiquidadoAposBaixaOuNaoRegistro
        else if AOcorrencia = '19' then
          Tipo := toRetornoRecebimentoInstrucaoProtestar
        else if AOcorrencia = '20' then
          Tipo := toRetornoRecebimentoInstrucaoSustarProtesto
        else if AOcorrencia = '23' then
          Tipo := toRetornoEntradaEmCartorio
        else if AOcorrencia = '24' then
          Tipo := toRetornoRetiradoDeCartorio
        else if AOcorrencia = '25' then
          Tipo := toRetornoBaixaPorProtesto
        else if AOcorrencia = '26' then
          Tipo := toRetornoInstrucaoRejeitada
        else if AOcorrencia = '27' then
          Tipo := toRetornoAlteracaoUsoCedente
        else if AOcorrencia = '28' then
          Tipo := toRetornoDebitoTarifas
        else if AOcorrencia = '29' then
          Tipo := toRetornoOcorrenciasDoSacado
        else if AOcorrencia = '30' then
          Tipo := toRetornoAlteracaoDadosRejeitados
      end;
    end;
  end;
begin
  // by Jéter Rabelo Ferreira - 06/2014
  iLinha := 0;

  // Verificar se o retorno é do banco selecionado
  if StrToIntDef(copy(ARetorno.Strings[0], 1, 3),-1) <> Numero then
    raise Exception.create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                           'não é um arquivo de retorno do banco' + sLineBreak + Nome));

  rCodigoCedente := Copy(ARetorno[0], 53, 9);
  rCedente       := Copy(ARetorno[0], 73, 30);
  rAgencia       := Copy(ARetorno[0], 33, 4);
  rAgenciaDigito := Copy(ARetorno[0], 37, 1);
  rConta         := PadLeft(OnlyNumber(Copy(ARetorno[0], 38, 9)), fpTamanhoConta, '0');
  rContaDigito   := Copy(ARetorno[0], 47, 1);
  rCNPJCPF       := RightStr(OnlyNumber(Copy(ARetorno[0], 18, 15)), 14);

  with ACBrBanco.ACBrBoleto do
  begin
    if (not LeCedenteRetorno) and (rCNPJCPF <> OnlyNumber(Cedente.CNPJCPF)) then
       raise Exception.create(ACBrStr('CNPJ\CPF do arquivo inválido'));

    if (not LeCedenteRetorno) and ((rAgencia <> OnlyNumber(Cedente.Agencia)) or
        (rConta <> OnlyNumber(Cedente.Conta))) then
       raise Exception.Create(ACBrStr('Agencia\Conta do arquivo inválido'));

    Cedente.Nome := rCedente;
    Cedente.CodigoCedente := rCodigoCedente;
    Cedente.CNPJCPF := rCnpjCpf;
    Cedente.Agencia := rAgencia;
    Cedente.AgenciaDigito := rAgenciaDigito;
    Cedente.Conta := rConta;
    Cedente.ContaDigito := rContaDigito;

    case StrToIntDef(copy(ARetorno[0], 17, 1), 0) of
      1:
        Cedente.TipoInscricao := pFisica;
      else
        Cedente.TipoInscricao := pJuridica;
    end;

    ACBrBanco.ACBrBoleto.ListadeBoletos.Clear;
  end;

  ACBrBanco.ACBrBoleto.DataArquivo := StringToDateTimeDef(Copy(ARetorno[0],144,2)+'/'+
                                                          Copy(ARetorno[0],146,2)+'/'+
                                                          Copy(ARetorno[0],148,4),0, 'DD/MM/YYYY' );

  ACBrBanco.ACBrBoleto.NumeroArquivo := StrToIntDef(Copy(ARetorno[0],158,6),0);

  ACBrBanco.TamanhoMaximoNossoNum := 13;

  for iLinha := 1 to ARetorno.Count - 2 do
  begin
    Linha := ARetorno[iLinha];

    if copy(Linha, 14, 1) = 'T' then // se for segmento T cria um novo Titulo
       Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

    with Titulo do
    begin
      if copy(Linha, 14, 1) = 'T' then
      begin
        NossoNumero := Copy(Linha, 41, ACBrBanco.TamanhoMaximoNossoNum);
        SeuNumero := Copy(Linha, 55, 15);
        NumeroDocumento := Copy(Linha, 55, 15);
        Carteira := Copy(Linha, 54, 1);
        ValorDocumento := StrToFloatDef(copy(Linha, 78, 15), 0) / 100;
        ValorDespesaCobranca := StrToFloatDef(copy(Linha, 194, 15), 0) / 100;
        // Sacado
        if Copy(Linha, 128, 1) = '1' then
          Sacado.Pessoa := pFisica
        else
          Sacado.Pessoa := pJuridica;
        Sacado.CNPJCPF := Trim(Copy(Linha, 129, 15));
        Sacado.NomeSacado := Trim(Copy(Linha, 144, 40));
      end
      else if copy(Linha, 14, 1) = 'U' then
      begin
        // Algumas ocorrências estão diferentes do cnab400, farei uma separada aqui
        DoVerOcorrencia(Copy(Linha, 16, 2));
        ValorDocumento := max(ValorDocumento,StrToFloatDef(copy(Linha, 78, 15), 0) / 100);
        ValorMoraJuros := StrToFloatDef(copy(Linha, 18, 15), 0) / 100;
        ValorDesconto := StrToFloatDef(copy(Linha, 33, 15), 0) / 100;
        ValorAbatimento := StrToFloatDef(copy(Linha, 48, 15), 0) / 100;
        ValorIOF := StrToFloatDef(copy(Linha, 63, 15), 0) / 100;
        ValorRecebido := StrToFloatDef(copy(Linha, 78, 15), 0) / 100;
        ValorOutrasDespesas := StrToFloatDef(copy(Linha, 108, 15), 0) / 100;
        ValorOutrosCreditos := StrToFloatDef(copy(Linha, 123, 15), 0) / 100;
        DataOcorrencia := StringToDateTimeDef(Copy(Linha, 138, 2)+'/'+
                                              Copy(Linha, 140, 2)+'/'+
                                              Copy(Linha, 142,4),0, 'DD/MM/YYYY' );
        DataCredito := StringToDateTimeDef(Copy(Linha, 146, 2)+'/'+
                                           Copy(Linha, 148, 2)+'/'+
                                           Copy(Linha, 150,4),0, 'DD/MM/YYYY' );
      end;
    end;
  end;
end;

Procedure TACBrBancoSantander.LerRetorno400 ( ARetorno: TStringList );
var
  Titulo : TACBrTitulo;
  ContLinha, CodOcorrencia, CodMotivo : Integer;
  Linha, rCedente, rAgencia, rConta, rDigitoConta, rCNPJCPF : String;
begin
   ContLinha := 0;

   if StrToIntDef(copy(ARetorno.Strings[0],77,3),-1) <> Numero then
      raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                             'não é um arquivo de retorno do '+ Nome));

   rCedente := trim(Copy(ARetorno[0],47,30));
   rAgencia := trim(Copy(ARetorno[1],18,4));
   rConta   := trim(Copy(ARetorno[1],22,8))+ Copy(ARetorno[1],384,1);
   rConta   := PadLeft( OnlyNumber(rConta),fpTamanhoConta,'0');
   rDigitoConta := Copy(ARetorno[1],385,1);

   rCNPJCPF := OnlyNumber( Copy(ARetorno[1],04,14) );

   with ACBrBanco.ACBrBoleto do
   begin
      if (not LeCedenteRetorno) and (rCNPJCPF <> OnlyNumber(Cedente.CNPJCPF)) then
         raise Exception.Create(ACBrStr('CNPJ\CPF do arquivo inválido'));

      if (not LeCedenteRetorno) and ((rAgencia <> OnlyNumber(Cedente.Agencia)) or
          (rConta <> OnlyNumber(Cedente.Conta))) then
         raise Exception.Create(ACBrStr('Agencia\Conta do arquivo inválido'));

      Cedente.Nome    := rCedente;
      Cedente.CNPJCPF := rCNPJCPF;
      Cedente.Agencia := rAgencia;
      Cedente.AgenciaDigito:= '0';
      Cedente.Conta   := rConta;
      Cedente.ContaDigito:= rDigitoConta;

      DataArquivo   := StringToDateTimeDef(Copy(ARetorno[0],95,2)+'/'+
                                           Copy(ARetorno[0],97,2)+'/'+
                                           Copy(ARetorno[0],99,2),0, 'DD/MM/YY' );

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
         SeuNumero   := copy(Linha,38,25);
         NossoNumero := Copy(Linha,63,08);
         Carteira    := Copy(Linha,108,1);

         OcorrenciaOriginal.Tipo := CodOcorrenciaToTipo(StrToIntDef(
                                                        copy(Linha,109,2),0));

         DataOcorrencia:= StringToDateTimeDef(Copy(Linha,111,2)+'/'+
                                              Copy(Linha,113,2)+'/'+
                                              Copy(Linha,115,2),0, 'DD/MM/YY' );

         NumeroDocumento:= Copy(Linha,117,10);

         CodOcorrencia := StrToIntDef(copy(Linha,135,2),0);

         //-|Se a ocorrencia for igual a > 0 - Houve Erros
         if(CodOcorrencia > 0) then
         begin
            if copy(Linha,137,3) <> '   ' then
            begin
               CodMotivo:= StrToIntDef(copy(Linha,137,3),0);
               MotivoRejeicaoComando.Add(copy(Linha,137,3));
               DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(
                                                  OcorrenciaOriginal.Tipo,CodMotivo));
            end;

            if copy(Linha,140,3) <> '   ' then
            begin
               CodMotivo:= StrToIntDef(copy(Linha,140,3),0);
               MotivoRejeicaoComando.Add(copy(Linha,137,3));
               DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(
                                                  OcorrenciaOriginal.Tipo,CodMotivo));
            end;

            if copy(Linha,143,3) <> '   ' then
            begin
               CodMotivo:= StrToIntDef(copy(Linha,143,3),0);
               MotivoRejeicaoComando.Add(copy(Linha,137,3));
               DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(
                                                  OcorrenciaOriginal.Tipo,CodMotivo));
            end;
         end;

         Vencimento := StringToDateTimeDef( Copy(Linha,147,2)+'/'+
                                            Copy(Linha,149,2)+'/'+
                                            Copy(Linha,151,2),0, 'DD/MM/YY' );

         ValorDocumento       := StrToFloatDef(Copy(Linha,153,13),0)/100;

         case StrToIntDef(Copy(Linha,174,2),0) of
            1: EspecieDoc:= 'DM';
            2: EspecieDoc:= 'NP';
            3: EspecieDoc:= 'NS';
            5: EspecieDoc:= 'RC';
            6: EspecieDoc:= 'DS';
            7: EspecieDoc:= 'LS';
         end;

         ValorDespesaCobranca := StrToFloatDef(Copy(Linha,176,13),0)/100;
         ValorOutrasDespesas  := StrToFloatDef(Copy(Linha,189,13),0)/100;
         ValorMoraJuros       := StrToFloatDef(Copy(Linha,202,13),0) +
                                 StrToFloatDef(Copy(Linha,267,13),0)/100;
         ValorIOF             := StrToFloatDef(Copy(Linha,215,13),0)/100;
         ValorAbatimento      := StrToFloatDef(Copy(Linha,228,13),0)/100;
         ValorDesconto        := StrToFloatDef(Copy(Linha,241,13),0)/100;
         ValorRecebido        := StrToFloatDef(Copy(Linha,254,13),0)/100;
         ValorOutrosCreditos  := StrToFloatDef(Copy(Linha,280,13),0)/100;

         if Copy(Linha,294,1) = 'N' then
            Aceite:=  atNao
         else
            Aceite:=  atSim;

         if StrToIntDef(Copy(Linha,296,6),0) <> 0 then
            DataCredito:= StringToDateTimeDef( Copy(Linha,296,2)+'/'+
                                               Copy(Linha,298,2)+'/'+
                                               Copy(Linha,300,2),0, 'DD/MM/YY' );

         Sacado.NomeSacado:= Copy(Linha,302,36);
      end;
   end;
end;

function TACBrBancoSantander.TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String;
var
 CodOcorrencia: Integer;
begin

  CodOcorrencia := StrToIntDef(TipoOCorrenciaToCod(TipoOcorrencia),0);

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
end;

function TACBrBancoSantander.CodOcorrenciaToTipo(const CodOcorrencia:
   Integer ) : TACBrTipoOcorrencia;
begin
   case CodOcorrencia of
      01: Result := toRetornoTituloNaoExiste;
      02: Result := toRetornoRegistroConfirmado;
      03: Result := toRetornoRegistroRecusado;
      06: Result := toRetornoLiquidado;
      07: Result := toRetornoLiquidadoPorConta;
      08: Result := toRetornoLiquidadoSaldoRestante;
      09: Result := toRetornoBaixaAutomatica;
      10: Result := toRetornoBaixadoInstAgencia;
      11: Result := toRetornoTituloEmSer;
      12: Result := toRetornoAbatimentoConcedido;
      13: Result := toRetornoAbatimentoCancelado;
      14: Result := toRetornoVencimentoAlterado;
      15: Result := toRetornoEncaminhadoACartorio;
      16: Result := toRetornoTituloJaBaixado;
      17: Result := toRetornoLiquidadoEmCartorio;
      21: Result := toREtornoEntradaEmCartorio;
      22: Result := toRetornoRetiradoDeCartorio;
      24: Result := toRetornoCustasCartorioDistribuidor;
      25: Result := toRetornoProtestado;
      26: Result := toRetornoProtestoSustado;
   else
      Result := toRetornoOutrasOcorrencias;
   end;
end;

function TACBrBancoSantander.TipoOCorrenciaToCod (
   const TipoOcorrencia: TACBrTipoOcorrencia ) : String;
begin
   case TipoOcorrencia of
      toRetornoRegistroConfirmado : Result:='02';
      toRetornoRegistroRecusado   : Result:='03';
      toRetornoLiquidado          : Result:='06';
      toRetornoBaixadoViaArquivo  : Result:='09';
      toRetornoBaixadoInstAgencia : Result:='10';
      toRetornoTituloEmSer        : Result:='11';
      toRetornoAbatimentoConcedido: Result:='12';
      toRetornoAbatimentoCancelado: Result:='13';
      toRetornoVencimentoAlterado : Result:='14';
      toRetornoLiquidadoEmCartorio: Result:='15';
      toRetornoTituloPagoemCheque : Result:='16';
      toRetornoLiquidadoAposBaixaouNaoRegistro : Result:= '17';
      toRetornoAcertoDepositaria  : Result:='18';
      toRetornoRecebimentoInstrucaoProtestar      : Result := '19';
      toRetornoRecebimentoInstrucaoSustarProtesto : Result := '20';
      toRetornoAcertoControleParticipante         : Result := '21';
      toRetornoRecebimentoInstrucaoAlterarDados   : Result := '22';
      toRetornoEncaminhadoACartorio               : Result := '23';
      toRetornoEntradaRejeitaCEPIrregular         : Result := '24';
      toRetornoBaixaRejeitada                     : Result := '27';
      toRetornoDebitoTarifas      : Result:='28';
      toRetornoOcorrenciasdoSacado                : Result := '29';
      toRetornoALteracaoOutrosDadosRejeitada      : Result := '30';
      toRetornoComandoRecusado                    : Result := '32';
      toRetornoDesagendamentoDebitoAutomatico     : Result := '35';
   else
      Result:= '02';
   end;
end;

function TACBrBancoSantander.COdMotivoRejeicaoToDescricao( const TipoOcorrencia:TACBrTipoOcorrencia ;CodMotivo: Integer) : String;
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
end;


end.


