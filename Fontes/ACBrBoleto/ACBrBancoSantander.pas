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
  Classes, SysUtils, Contnrs,
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

    function CodOcorrenciaToTipoRemessa(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
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
   fpDigito                 := 7;
   fpNome                   := 'Santander';
   fpNumero                 := 033;
   fpTamanhoMaximoNossoNum  := 12;
   fpTamanhoCarteira        := 3;
   fpTamanhoConta           := 11;
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
  sCodMovimento, sAgencia, sCCorrente: String;
  sDigitoNossoNumero, sTipoCobranca, sTipoDocto, sTipoCarteira: String;
  sEspecie, sDataMoraJuros, sDataDesconto: String;
  STipoJuros, sTipoDesconto, sDiasProtesto, sDiasBaixaDevol: String;
  sTipoInscricao, sEndereco : String;
  aTipoInscricao: Char;
  ACodProtesto: Char;
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
 aTipoInscricao := ' ';

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
       toRemessaAlterarOutrosDados            : sCodMovimento := '31'; {Alteração de outros dados}
       toRemessaNaoProtestar                  : sCodMovimento := '98'; {Não Protestar (Antes de iniciar o ciclo de protesto )}
    else
       sCodMovimento := '01';                                          {Remessa}
    end;

    sAgencia := PadLeft(OnlyNumber(ACBrTitulo.ACBrBoleto.Cedente.Agencia) +
                        ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito,5,'0');

    // Tamanho da conta corrente definida como padrão de 11 digitos, porém no arquivo
    // remessa a conta solicitada é de 8 dígitos.
    // Devemos retirar os zeros a esquerda da conta

    sCCorrente := OnlyNumber(ACBrTitulo.ACBrBoleto.Cedente.Conta);
    sCCorrente := Copy(SCCorrente, Length(sCCorrente) - 8, 9) +
                  OnlyNumber(ACBrTitulo.ACBrBoleto.Cedente.ContaDigito);

    sDigitoNossoNumero := CalcularDigitoVerificador(ACBrTitulo);

    case ACBrBoleto.Cedente.CaracTitulo of      
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
    else if Trim(EspecieDoc) = 'ND' then {ND - NOTA PROMISSORIA DIRETA}
      sEspecie := '98'
    else
    begin
      if not MatchText(EspecieDoc, ['02', '04', '12', '13', '17', '20', '97', '98']) then
        raise Exception.Create('Espécie de documento informada incorretamente!');

      sEspecie := EspecieDoc;
    end;

    if (ValorMoraJuros > 0) then
    begin
      STipoJuros := IfThen( (CodigoMora <> ''), CodigoMora, '1');    //1- Valor por dia  2- Taxa Mensal
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

    // 0 = ISENTO
    // 1 = Valor fixo ate a data informada – Informar o valor no campo “valor de desconto a ser concedido.
    // 2 = Percentual ate a data informada – Informar o percentual no campo “percentual de desconto a ser concedido
    // 3 = Valor por antecipação por dia corrido - Informar o valor no campo “valor de desconto a ser concedido
    // 4 = Valor por antecipação dia útil - Informar o valor no campo “valor de desconto a ser concedido
    case ACBrTitulo.TipoDesconto of
         tdNaoConcederDesconto: sTipoDesconto := '0';
         tdValorFixoAteDataInformada: sTipoDesconto := '1';
         tdPercentualAteDataInformada: sTipoDesconto := '2';
    end;

    if ValorDesconto > 0 then
    begin
      if DataDesconto <> 0 then
        sDataDesconto := FormatDateTime('ddmmyyyy', DataDesconto)
      else
        sDataDesconto := PadLeft('', 8, '0');
    end
    else
       sDataDesconto := PadLeft('', 8, '0');

    {Código para Protesto}
    case TipoDiasProtesto of
       diCorridos       : ACodProtesto := '1';
       diUteis          : ACodProtesto := '2';
    else
       ACodProtesto := '0';
    end;

    {Instruções}

    Instrucao1 := Trim(Instrucao1);
    Instrucao2 := Trim(Instrucao2);

    if ((DataProtesto <> 0) and (DiasDeProtesto > 0)) then
    begin
      if not MatchText(Instrucao1, ['0', '1', '2', '3', '9']) then
        Instrucao1 := ACodProtesto;
      // Dias para protesto
      sDiasProtesto := PadLeft(IntToStr(DiasDeProtesto), 2, '0');
    end
    else
    begin
      Instrucao1 := '0';  // Não protestar
      SDiasProtesto := '00';
    end;

    // Baixa/Devolução
    if (Instrucao2 = '') then
      Instrucao2 := '2' // NAO BAIXAR / NAO DEVOLVER
    else
    begin
      if not MatchText(Instrucao2, ['1', '2', '3']) then
        raise Exception.Create('Código de Baixa/Devolução informado incorretamente!');
    end;

    sDiasBaixaDevol:= ifthen(DataBaixa > 0,
                             IntToStrZero(DaysBetween(Vencimento,DataBaixa),2),
                             '00');

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
    Result := '033'                                                   + // 001 - 003 / Código do Banco na compensação
              '0001'                                                  + // 004 - 007 / Numero do lote remessa
              '3'                                                     + // 008 - 008 / Tipo de registro
              IntToStrZero(ISequencia ,5)                             + // 009 - 013 / Número seqüencial do registro no lote
              'P'                                                     + // 014 - 014 / Cód. Segmento do registro detalhe
              Space(1)                                                + // 015 - 015 / Reservado (uso Banco)
              sCodMovimento                                           + // 016 - 017 / Código de movimento remessa
              Copy(sAgencia, 1, 4)                                    + // 018 – 021 / Agência do Cedente
              Copy(sAgencia, 5, 1)                                    + // 022 – 022 / Dígito da Agência do Cedente
              Copy(sCCorrente, 1, 9)                                  + // 023 - 031 / da conta corrente
              Copy(sCCorrente, 10, 1)                                 + // 032 – 032 / Dígito verificador da conta
              Copy(sCCorrente, 1, 9)                                  + // 033 - 041 / Conta cobrança
              Copy(sCCorrente, 10, 1)                                 + // 042 - 042 / Dígito da conta cobrança
              Space(2)                                                + // 043 - 044 / Reservado (uso Banco)
              NossoNumero + sDigitoNossoNumero                        + // 045 – 057 / Identificação do título no Banco (Nosso Número
              sTipoCobranca                                           + // 058 - 058 / Tipo de cobrança
              sTipoCarteira                                           + // 059 - 059 / Forma de Cadastramento = 1 Registrada / 2 Sem Registro
              sTipoDocto                                              + // 060 - 060 / Tipo de documento
              Space(1)                                                + // 061 - 061 / Reservado (uso Banco)
              Space(1)                                                + // 062 - 062 / Reservado (uso Banco)
              PadRight(Copy(NumeroDocumento, 1, 15), 15)              + // 063 - 077 / Nº do documento              
              FormatDateTime('ddmmyyyy',Vencimento)                   + // 078 - 085 / Data de vencimento do título
              IntToStrZero(round(ValorDocumento * 100), 15)           + // 086 - 100 / Valor nominal do título
              PadLeft('0', 4, '0')                                    + // 101 - 104 / Agência encarregada da cobrança
              '0'                                                     + // 105 - 105 / Dígito da Agência encarregada da cobrança
              Space(1)                                                + // 106 - 106 / Reservado (uso Banco)
              sEspecie                                                + // 107 – 108 / Espécie do título
              ifThen(Aceite = atSim,  'A', 'N')                       + // 109 - 109 / Identif. de título Aceito/Não Aceito
              FormatDateTime('ddmmyyyy',DataDocumento)                + // 110 - 117 / Data da emissão do título
              STipoJuros                                              + // 118 - 118 / Código do juros de mora
              sDataMoraJuros                                          + // 119 - 126 / Data do juros de mora
              IntToStrZero(round(ValorMoraJuros * 100), 15)           + // 127 - 141 / Valor da mora/dia ou Taxa mensal
              sTipoDesconto                                           + // 142 - 142 / Código do desconto 1
              sDataDesconto                                           + // 143 - 150 / Data de desconto 1
              IntToStrZero(round(ValorDesconto * 100), 15)            + // 151 - 165 / Valor ou Percentual do desconto concedido
              IntToStrZero(round(ValorIOF * 100), 15)                 + // 166 - 180 / Valor do IOF a ser recolhido
              IntToStrZero(round(ValorAbatimento * 100), 15)          + // 181 - 195 / Valor do abatimento
              PadRight(SeuNumero, 25)                                 + // 196 - 220 / Identificação do título na empresa              
              Instrucao1                                              + // 221 - 221 / Código para protesto
              sDiasProtesto                                           + // 222 - 223 / Número de dias para protesto
              Instrucao2                                              + // 224 - 224 / Código para Baixa/Devolução
              '0'                                                     + // 225 - 225 / Reservado (uso Banco)
              sDiasBaixaDevol                                         + // 226 - 227 / Número de dias para Baixa/Devolução
              '00'                                                    + // 228 - 229 / Código da moeda
              Space(11)                                               ; // 230 – 240 / Reservado (uso Banco)
    {SEGMENTO P - FIM}

    Inc(ISequencia);
    if sCodMovimento = '01' then
     begin
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
              PadRight(Sacado.UF, 2)                                 + // 152 - 153 / Unidade da federação do sacado
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
              PadLeft('', 8, '0')                                        + // 019 - 026 / Data do desconto 2
              IntToStrZero(0, 15)                                        + // 027 - 041 / Valor/Percentual a ser concedido
              Space(24)                                                  + // 042 – 065 / Reservado (uso Banco)
              IfThen((PercentualMulta > 0),
                     IfThen(MultaValorFixo,'1','2'), '0')                                           + // 66 - 66 1-Cobrar Multa Valor Fixo / 2-Percentual / 0-Não cobrar multa
              IfThen((PercentualMulta > 0),
                      FormatDateTime('ddmmyyyy', DataMulta), '00000000')                            + // 67 - 74 Se cobrar informe a data para iniciar a cobrança ou informe zeros se não cobrar
              IfThen((PercentualMulta > 0), IntToStrZero(round(PercentualMulta * 100), 15),
                     PadRight('', 15, '0'))                                                         + // 075 - 089 / Valor/Percentual a ser aplicado
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
         toRemessaNaoProtestar                  : Ocorrencia := '98'; {Sustar protesto antes do início do ciclo de protesto}
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
      else if trim(EspecieDoc) = 'BDP' then
         aEspecie:= '08'
      else if trim(EspecieDoc) = 'BCC' then
         aEspecie:= '19'
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
         wLinha:= '1'                                                         +  // 1- ID Registro
                  IfThen(Cedente.TipoInscricao = pJuridica,'02','01')         +  // 2 a 3
                  PadLeft(trim(OnlyNumber(Cedente.CNPJCPF)),14,'0')           +  // 4 a 17
                  PadRight(trim(Cedente.CodigoTransmissao),20,'0')            +  // 18 a 37
                  PadRight( SeuNumero ,25,' ')                                +  // 38 a 62
                  PadLeft(RightStr(NossoNumero,7),7,'0') + DigitoNossoNumero  +  // 63 a 70
                  IfThen(DataAbatimento < EncodeDate(2000,01,01),
                         '000000',
                         FormatDateTime( 'ddmmyy', DataAbatimento))           +  // 71 a 76
                  ' '+IfThen(PercentualMulta > 0,'4','0')                     +  // 77 a 78
                  IntToStrZero( round( PercentualMulta * 100 ), 4)            +  // 79 a 82
                  '00'+StringOfChar( '0', 13)+space(4)                        +  // 83 a 101
                  IfThen((DataMulta <= 0),'000000',
                         FormatDateTime( 'ddmmyy', DataMulta))                +  // 102 a 107
                   IntToStr(aCarteira) + Ocorrencia                           +  // 108 a 110
                  PadRight( NumeroDocumento,10,' ')                           +  // 111 a 120
                  FormatDateTime( 'ddmmyy', Vencimento)                       +  // 121 a 126
                  IntToStrZero( round( ValorDocumento * 100), 13)             +  // 127 a 139
                  '033' + aAgencia                                            +  // 140 a 147
                  PadLeft(aEspecie, 2) + 'N'                                  +  // 148 a 150
                  FormatDateTime( 'ddmmyy', DataDocumento )                   +  // 151 a 156
                  PadLeft(trim(Instrucao1),2,'0')                             +  // 157 a 158
                  PadLeft(trim(Instrucao2),2,'0')                             +  // 159 a 160
                  IntToStrZero( round(ValorMoraJuros * 100 ), 13)             +  // 161 a 173
                  IfThen(DataDesconto < EncodeDate(2000,01,01),
                         '000000',
                         FormatDateTime( 'ddmmyy', DataDesconto))             +  // 174 a 179
                  IntToStrZero( round( ValorDesconto * 100), 13)              +  // 180 a 192
                  IntToStrZero( round( ValorIOF * 100 ), 13)                  +  // 193 a 205
                  IntToStrZero( round( ValorAbatimento * 100 ), 13)           +  // 206 a 218
                  TipoSacado + PadLeft(OnlyNumber(Sacado.CNPJCPF),14,'0')     +  // 219 a 234
                  PadRight( Sacado.NomeSacado, 40, ' ')                       +  // 235 a 274
                  PadRight( Sacado.Logradouro + ' '+ Sacado.Numero, 40, ' ')  +  // 275 a 314
                  PadRight( Sacado.Bairro,12,' ')                             +  // 315 a 326
                  PadRight( OnlyNumber(Sacado.CEP) , 8, ' ' )                 +  // 327 a 334
                  PadRight( Sacado.Cidade, 15, ' ')                           +
                  PadRight( Sacado.UF, 2 )                                    +  // 335 a 351
                  Space(30)                                                   +  // 352 a 381
                  ' I'                                                        +  // 382 a 383
                  Copy( Cedente.Conta, length( Cedente.Conta ),1 )            +  //
                  PadLeft( Cedente.ContaDigito, 1 )                           +  // 384 a 385
                  Space(6)                                                    +  // 386 a 391
                  Protesto + ' '                                              +  // 392 a 394
                  IntToStrZero( aRemessa.Count + 1, 6 );                         // 395 a 400


         wLinha:= UpperCase(wLinha);

         for I := 0 to Mensagem.count-1 do
            wLinha:= wLinha + #13#10                         +
                     '2' + space(16)                             +
                     PadLeft(Cedente.CodigoTransmissao,20,'0')   +
                     Space(10) + '01'                            +
                     PadRight(Mensagem[I],50)                        +
                     Space(283) + 'I'                            +
                     Copy( Cedente.Conta, length( Cedente.Conta ),1 )   + 
                     PadLeft( Cedente.ContaDigito, 1 )                           +
                     Space(9)                                           +
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

  procedure DoVerOcorrencia(AOcorrencia: string);
  var
    pMotivoRejeicao, CodMotivo, I: Integer;
  begin
    with Titulo.OcorrenciaOriginal do
    begin
      if MatchText(AOcorrencia, ['03', '06', '09', '17', '26', '30'])  then
      begin
       if AOcorrencia = '03' then
         Tipo:= toRetornoRegistroRecusado
       else if AOcorrencia = '26' then
         Tipo := toRetornoInstrucaoRejeitada
       else if AOcorrencia = '30' then
         Tipo := toRetornoAlteracaoDadosRejeitados
       else if MatchText(AOcorrencia, ['06', '09'])  then
         Tipo := CodOcorrenciaToTipo(StrToInt(AOcorrencia))
       else if AOcorrencia = '17' then
          Tipo := toRetornoLiquidadoAposBaixaOuNaoRegistro;
       pMotivoRejeicao:= 209;
       for I:= 0 to 4 do
       begin
         CodMotivo:= StrToIntDef(copy(Linha,pMotivoRejeicao,2),0);
         if CodMotivo > 0 then
         begin
           Titulo.MotivoRejeicaoComando.Add(copy(Linha, pMotivoRejeicao, 2));
           Titulo.DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(
                                                     Titulo.OcorrenciaOriginal.Tipo,CodMotivo));
         end;
         Inc(pMotivoRejeicao, 2);
       end;
      end
      else if MatchText(AOcorrencia, ['02', '06', '09', '11', '12', '13', '14'])  then
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
        else if AOcorrencia = '27' then
          Tipo := toRetornoAlteracaoUsoCedente
        else if AOcorrencia = '28' then
          Tipo := toRetornoDebitoTarifas
        else if AOcorrencia = '29' then
          Tipo := toRetornoOcorrenciasDoSacado        
        else if AOcorrencia = '32' then
          Tipo := toRetornoIOFInvalido
        else if AOcorrencia = '51' then
          Tipo := toRetornoTituloDDAReconhecidoPagador
        else if AOcorrencia = '52' then
          Tipo := toRetornoTituloDDANaoReconhecidoPagador
        else if AOcorrencia = '53' then
          Tipo := toRetornoTituloDDARecusadoCIP;
      end;
    end;
  end;
begin

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

  case StrToIntDef(Copy(ARetorno[0],17,1),0) of
    1: rCNPJCPF := Copy(ARetorno[0],22,11);
    2: rCNPJCPF := Copy(ARetorno[0],19,14);
  else
   rCNPJCPF := Copy(ARetorno[0],19,14);
  end;

  ValidarDadosRetorno(rAgencia, rConta, rCNPJCPF);
  with ACBrBanco.ACBrBoleto do
  begin
    Cedente.Nome          := rCedente;
    Cedente.CodigoCedente := rCodigoCedente;
    Cedente.CNPJCPF       := rCnpjCpf;
    Cedente.Agencia       := rAgencia;
    Cedente.AgenciaDigito := rAgenciaDigito;
    Cedente.Conta         := rConta;
    Cedente.ContaDigito   := rContaDigito;

    if StrToIntDef(copy(ARetorno[0], 17, 1), 0) = 1 then
      Cedente.TipoInscricao := pFisica
    else
      Cedente.TipoInscricao := pJuridica;

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
        NossoNumero          := Copy(Linha, 41, ACBrBanco.TamanhoMaximoNossoNum);
        NumeroDocumento      := Copy(Linha, 55, 15);
        SeuNumero            := Copy(Linha, 101, 25);        
        Carteira             := Copy(Linha, 54, 1);
        Vencimento           := StringToDateTimeDef(Copy(Linha, 70, 2)+'/'+
                                                    Copy(Linha, 72, 2)+'/'+
                                                    Copy(Linha, 74,4),0, 'DD/MM/YYYY' );
        ValorDocumento       := StrToFloatDef(copy(Linha, 78, 15), 0) / 100;
        ValorDespesaCobranca := StrToFloatDef(copy(Linha, 194, 15), 0) / 100;
        // Sacado
        if Copy(Linha, 128, 1) = '1' then
        begin
          Sacado.Pessoa  := pFisica;
          Sacado.CNPJCPF := Trim(Copy(Linha, 133, 11));
        end
        else
        begin
          Sacado.Pessoa := pJuridica;
          Sacado.CNPJCPF    := Trim(Copy(Linha, 129, 15));
        end;
        Sacado.NomeSacado := Trim(Copy(Linha, 144, 40));

        // Algumas ocorrências estão diferentes do cnab400, farei uma separada aqui
        DoVerOcorrencia(Copy(Linha, 16, 2));
      end
      else if copy(Linha, 14, 1) = 'U' then
      begin
        ValorDocumento      := max(ValorDocumento,StrToFloatDef(copy(Linha, 78, 15), 0) / 100);
        ValorMoraJuros      := StrToFloatDef(copy(Linha, 18, 15), 0) / 100;
        ValorDesconto       := StrToFloatDef(copy(Linha, 33, 15), 0) / 100;
        ValorAbatimento     := StrToFloatDef(copy(Linha, 48, 15), 0) / 100;
        ValorIOF            := StrToFloatDef(copy(Linha, 63, 15), 0) / 100;
        ValorRecebido       := StrToFloatDef(copy(Linha, 78, 15), 0) / 100;
        ValorOutrasDespesas := StrToFloatDef(copy(Linha, 108, 15), 0) / 100;
        ValorOutrosCreditos := StrToFloatDef(copy(Linha, 123, 15), 0) / 100;
        DataOcorrencia      := StringToDateTimeDef(Copy(Linha, 138, 2)+'/'+
                                                   Copy(Linha, 140, 2)+'/'+
                                                   Copy(Linha, 142,4),0, 'DD/MM/YYYY' );
        DataCredito := StringToDateTimeDef(Copy(Linha, 146, 2)+'/'+
                                           Copy(Linha, 148, 2)+'/'+
                                           Copy(Linha, 150,4),0, 'DD/MM/YYYY' );
      end;
    end;
  end;
  ACBrBanco.TamanhoMaximoNossoNum := 12;
end;

Procedure TACBrBancoSantander.LerRetorno400 ( ARetorno: TStringList );
var
  Titulo : TACBrTitulo;
  ContLinha, CodOcorrencia, CodMotivo : Integer;
  Linha, rCedente, rAgencia, rConta, rDigitoConta, rCNPJCPF : String;
  wCodBanco: Integer;
begin   
   wCodBanco := StrToIntDef(copy(ARetorno.Strings[0],77,3),-1);
   if (wCodBanco <> Numero) and (wCodBanco <> 353) then
      raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                             'não é um arquivo de retorno do '+ Nome));

   rCedente := trim(Copy(ARetorno[0],47,30));
   rAgencia := trim(Copy(ARetorno[1],18,4));
   rConta   := trim(Copy(ARetorno[1],22,8))+ Copy(ARetorno[1],384,1);
   rConta   := PadLeft( OnlyNumber(rConta),fpTamanhoConta,'0');
   rDigitoConta := Copy(ARetorno[1],385,1);

   case StrToIntDef(Copy(ARetorno[1],2,2),0) of
      01: rCNPJCPF := Copy(ARetorno[1],7,11);
      02: rCNPJCPF := Copy(ARetorno[1],4,14);
   else
     rCNPJCPF := Copy(ARetorno[1],4,14);
   end;

   ACBrBanco.ACBrBoleto.DataCreditoLanc :=
     StringToDateTimeDef(Copy(ARetorno[0], 95, 2) + '/' +
                         Copy(ARetorno[0], 97, 2) + '/' +
                         Copy(ARetorno[0], 99, 2), 0, 'dd/mm/yy');

   ValidarDadosRetorno(rAgencia, rConta, rCNPJCPF);
   with ACBrBanco.ACBrBoleto do
   begin
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
         NossoNumero := Copy(Linha,63,07);
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
            01 : EspecieDoc:= 'DM';
            02 : EspecieDoc:= 'NP';
            03 : EspecieDoc:= 'NS';
            05 : EspecieDoc:= 'RC';
            06 : EspecieDoc:= 'DS';
            07 : EspecieDoc:= 'LS';
            08 : EspecieDoc:= 'BDP';
            19 : EspecieDoc:= 'BCC';
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
  Result := '';
  CodOcorrencia := StrToIntDef(TipoOCorrenciaToCod(TipoOcorrencia),0);

  { Atribuindo Ocorrências divergêntes entre CNAB240 e CNAB400 }
  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
  begin
    case CodOcorrencia of
      17: Result := '17-Liq. Após Baixa/Liq.Tít. não Registrado';
      24: Result := '24-Retirada de Cartório/Manutenção em Carteira';
      25: Result := '25-Protestado e Baixado';
      26: Result := '26-Instrução Rejeitada';
      51: Result := '51-Título DDA Reconhecido Pelo Sacado';
      52: Result := '52-Título DDA Não Reconhecido Pelo Sacado';
      53: Result := '53-Título DDA Recusado Pela CIP';
    end;
  end
  else
  begin
    case CodOcorrencia of
      17: Result := '17-Liquidado em Cartório';
      24: Result := '24-Custas de Cartório';
      25: Result := '25-Protestar Título';
      26: Result := '26-Sustar Protesto';
      35: Result := '35-Título DDA Reconhecido Pelo Sacado';
      36: Result := '36-Título DDA Não Reconhecido Pelo Sacado';
      37: Result := '37-Título DDA Recusado Pela CIP';
    end;
  end;

  if (Result <> '') then
    Exit;

  case CodOcorrencia of
    01: Result := '01-Título Não Existe';
    02: Result := '02-Entrada Tít.Confirmada';
    03: Result := '03-Entrada Tít.Rejeitada';
    04: Result := '04-Transf. de Carteira/Entrada';
    05: Result := '05-Transf. de Carteira/Baixa';
    06: Result := '06-Liquidação';
    07: Result := '07-Liquidação por Conta';
    08: Result := '08-Liquidação por Saldo';
    09: Result := '09-Baixa Automática';
    10: Result := '10-Tít.Baix.Conf.Instrução';
    11: Result := '11-Em Ser';
    12: Result := '12-Abatimento Concedido';
    13: Result := '13-Abatimento Cancelado';
    14: Result := '14-Prorrogação de Vencimento';
    15: Result := '15-Confirmação de Protesto';
    16: Result := '16-Tít.Já Baixado/Liquidado';
    19: Result := '19-Recebimento da Instrução Protesto';
    20: Result := '20-Recebimento da Instrução Não Protestar';
    21: Result := '21-Tít. Enviado a Cartório';
    22: Result := '22-Tít. Retirado de Cartório';
    23: Result := '23-Remessa a Cartório';
    27: Result := '27-Confirmação alt.de outros dados';
    28: Result := '28-Débito de tarifas e custas';
    29: Result := '29-Ocorrência do sacado';
    30: Result := '30-Alteração de dados rejeitada';
    32: Result := '32-Código IOF Inválido';
    38: Result := '38-Recebimento da Instrução Não Protestar'
  end;
end;

function TACBrBancoSantander.CodOcorrenciaToTipo(const CodOcorrencia:
   Integer ) : TACBrTipoOcorrencia;
begin
  // DONE -oJacinto Junior: Ajustar para utilizar as ocorrências corretas.
  Result := toTipoOcorrenciaNenhum;

  { Atribuindo Ocorrências divergêntes entre CNAB240 e CNAB400 }
  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
  begin
    case CodOcorrencia of
      17: Result := toRetornoLiquidadoAposBaixaOuNaoRegistro;
      24: Result := toRetornoRetiradoDeCartorio;
      25: Result := toRetornoProtestado;
      26: Result := toRetornoInstrucaoRejeitada;
      35: Result := toRetornoTituloDDAReconhecidoPagador;
      36: Result := toRetornoTituloDDANaoReconhecidoPagador;
      37: Result := toRetornoTituloDDARecusadoCIP;
    end;
  end
  else
  begin
    case CodOcorrencia of
      17: Result := toRetornoLiquidadoEmCartorio;
      24: Result := toRetornoCustasCartorio;
      25: Result := toRetornoRecebimentoInstrucaoProtestar;
      26: Result := toRetornoRecebimentoInstrucaoSustarProtesto;
      51: Result := toRetornoTituloDDAReconhecidoPagador;
      52: Result := toRetornoTituloDDANaoReconhecidoPagador;
      53: Result := toRetornoTituloDDARecusadoCIP;
    end;
  end;

  if (Result <> toTipoOcorrenciaNenhum) then
    Exit;

  case CodOcorrencia of
    01: Result := toRetornoTituloNaoExiste;
    02: Result := toRetornoRegistroConfirmado;
    03: Result := toRetornoRegistroRecusado;
    04: Result := toRetornoTransferenciaCarteiraEntrada;
    05: Result := toRetornoTransferenciaCarteiraBaixa;
    06: Result := toRetornoLiquidado;
    07: Result := toRetornoLiquidadoPorConta;
    08: Result := toRetornoLiquidadoSaldoRestante;
    09: Result := toRetornoBaixaAutomatica;
    10: Result := toRetornoBaixadoInstAgencia;
    11: Result := toRetornoTituloEmSer;
    12: Result := toRetornoAbatimentoConcedido;
    13: Result := toRetornoAbatimentoCancelado;
    14: Result := toRetornoVencimentoAlterado;
    15: Result := toRetornoProtestado;
    16: Result := toRetornoTituloJaBaixado;
    19: Result := toRetornoRecebimentoInstrucaoProtestar;
    20: Result := toRetornoRecebimentoInstrucaoSustarProtesto;
    21: Result := toRetornoEncaminhadoACartorio;
    22: Result := toRetornoRetiradoDeCartorio;
    23: Result := toRetornoEntradaEmCartorio;
    27: Result := toRetornoAlteracaoUsoCedente;
    28: Result := toRetornoDebitoTarifas;
    29: Result := toRetornoOcorrenciasDoSacado;
    30: Result := toRetornoAlteracaoDadosRejeitados;
    32: Result := toRetornoIOFInvalido;
    38: Result := toRetornoRecebimentoInstrucaoNaoProtestar;
  else
    Result := toRetornoOutrasOcorrencias;
  end;
end;

function TACBrBancoSantander.CodOcorrenciaToTipoRemessa(const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02 : Result:= toRemessaBaixar;                          {Pedido de Baixa}
    04 : Result:= toRemessaConcederAbatimento;              {Concessão de Abatimento}
    05 : Result:= toRemessaCancelarAbatimento;              {Cancelamento de Abatimento concedido}
    06 : Result:= toRemessaAlterarVencimento;               {Alteração de vencimento}
    07 : Result:= toRemessaAlterarControleParticipante;     {Alteração do controle do participante}
    08 : Result:= toRemessaAlterarNumeroControle;           {Alteração de seu número}
    09 : Result:= toRemessaProtestar;                       {Pedido de protesto}
    18 : Result:= toRemessaCancelarInstrucaoProtesto;       {Sustar protesto e manter na carteira}
    98 : Result:= toRemessaNaoProtestar;                    {Sustar protesto antes do início do ciclo de protesto}
  else
     Result:= toRemessaRegistrar;                           {Remessa}
  end;
end;

function TACBrBancoSantander.TipoOCorrenciaToCod (
   const TipoOcorrencia: TACBrTipoOcorrencia ) : String;
begin
  Result := '';

  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
  begin
    case TipoOcorrencia of
      toRetornoLiquidadoAposBaixaOuNaoRegistro               : Result := '17';
      toRetornoRetiradoDeCartorio                            : Result := '24';
      toRetornoProtestado                                    : Result := '25';
      toRetornoInstrucaoRejeitada                            : Result := '26';
      toRetornoTituloDDAReconhecidoPagador                   : Result := '35';
      toRetornoTituloDDANaoReconhecidoPagador                : Result := '36';
      toRetornoTituloDDARecusadoCIP                          : Result := '37';
    end;
  end
  else
  begin
    case TipoOcorrencia of
      toRetornoLiquidadoEmCartorio                           : Result := '17';
      toRetornoCustasCartorio                                : Result := '24';
      toRetornoRecebimentoInstrucaoProtestar                 : Result := '25';
      toRetornoRecebimentoInstrucaoSustarProtesto            : Result := '26';
      toRetornoTituloDDAReconhecidoPagador                   : Result := '51';
      toRetornoTituloDDANaoReconhecidoPagador                : Result := '52';
      toRetornoTituloDDARecusadoCIP                          : Result := '53';
    end;
  end;

  if (Result <> '') then
    Exit;

  case TipoOcorrencia of
    toRetornoTituloNaoExiste                                 : Result := '01';
    toRetornoRegistroConfirmado                              : Result := '02';
    toRetornoRegistroRecusado                                : Result := '03';
    toRetornoTransferenciaCarteiraEntrada                    : Result := '04';
    toRetornoTransferenciaCarteiraBaixa                      : Result := '05';
    toRetornoLiquidado                                       : Result := '06';
    toRetornoLiquidadoPorConta                               : Result := '07';
    toRetornoLiquidadoSaldoRestante                          : Result := '08';
    toRetornoBaixaAutomatica                                 : Result := '09';
    toRetornoBaixadoInstAgencia                              : Result := '10';
    toRetornoTituloEmSer                                     : Result := '11';
    toRetornoAbatimentoConcedido                             : Result := '12';
    toRetornoAbatimentoCancelado                             : Result := '13';
    toRetornoVencimentoAlterado                              : Result := '14';
    toRetornoProtestado                                      : Result := '15';
    toRetornoTituloJaBaixado                                 : Result := '16';
    toRetornoRecebimentoInstrucaoProtestar                   : Result := '19';
    toRetornoRecebimentoInstrucaoSustarProtesto              : Result := '20';
    toRetornoEncaminhadoACartorio                            : Result := '21';
    toRetornoRetiradoDeCartorio                              : Result := '22';
    toRetornoEntradaEmCartorio                               : Result := '23';
    toRetornoAlteracaoUsoCedente                             : Result := '27';
    toRetornoDebitoTarifas                                   : Result := '28';
    toRetornoOcorrenciasDoSacado                             : Result := '29';
    toRetornoAlteracaoDadosRejeitados                        : Result := '30';
    toRetornoIOFInvalido                                     : Result := '32';
    toRetornoRecebimentoInstrucaoNaoProtestar                : Result := '38';
  else
    Result := '02';
  end;
end;

function TACBrBancoSantander.CodMotivoRejeicaoToDescricao( const TipoOcorrencia:TACBrTipoOcorrencia; CodMotivo: Integer) : String;
begin
  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c400) then
  begin
    case CodMotivo of
      001: Result := '001-NOSSO NUMERO NAO NUMERICO';
      002: Result := '002-VALOR DO ABATIMENTO NAO NUMERICO';
      003: Result := '003-DATA VENCIMENTO NAO NUMERICA';
      004: Result := '004-CONTA COBRANCA NAO NUMERICA';
      005: Result := '005-CODIGO DA CARTEIRA NAO NUMERICO';
      006: Result := '006-CODIGO DA CARTEIRA INVALIDO';
      007: Result := '007-ESPECIE DO DOCUMENTO INVALIDA';
      008: Result := '008-UNIDADE DE VALOR NAO NUMERICA';
      009: Result := '009-UNIDADE DE VALOR INVALIDA';
      010: Result := '010-CODIGO PRIMEIRA INSTRUCAO NAO NUMERICA';
      011: Result := '011-CODIGO SEGUNDA INSTRUCAO NAO NUMERICA';
      012: Result := '012-VALOR DO TITULO EM OUTRA UNIDADE';
      013: Result := '013-VALOR DO TITULO NAO NUMERICO';
      014: Result := '014-VALOR DE MORA NAO NUMERICO';
      015: Result := '015-DATA EMISSAO NÃO NUMERICA';
      016: Result := '016-DATA DE VENCIMENTO INVALIDA';
      017: Result := '017-CODIGO DA AGENCIA COBRADORA NAO NUMERICA';
      018: Result := '018-VALOR DO IOC NAO NUMERICO';
      019: Result := '019-NUMERO DO CEP NAO NUMERICO';
      020: Result := '020-TIPO INSCRICAO NAO NUMERICO';
      021: Result := '021-NUMERO DO CGC OU CPF NAO NUMERICO';
      022: Result := '022-CODIGO OCORRENCIA INVALIDO';
      024: Result := '024-TOTAL PARCELA NAO NUMERICO';
      025: Result := '025-VALOR DESCONTO NAO NUMERICO';
      026: Result := '026-CODIGO BANCO COBRADOR INVALIDO';
      027: Result := '027-NUMERO PARCELAS CARNE NAO NUMERICO';
      028: Result := '028-NUMERO PARCELAS CARNE ZERADO';
      029: Result := '029-VALOR DE MORA INVALIDO';
      030: Result := '030-DT VENC MENOR DE 15 DIAS DA DT PROCES';
      039: Result := '039-PERFIL NAO ACEITA TITULO EM BCO CORRESP';
      041: Result := '041-AGENCIA COBRADORA NAO ENCONTRADA';
      042: Result := '042-CONTA COBRANCA INVALIDA';
      043: Result := '043-NAO BAIXAR,  COMPL. INFORMADO INVALIDO';
      044: Result := '044-NAO PROTESTAR, COMPL. INFORMADO INVALIDO';
      045: Result := '045-QTD DE DIAS DE BAIXA NAO PREENCHIDO';
      046: Result := '046-QTD DE DIAS PROTESTO NAO PREENCHIDO';
      047: Result := '047-TOT PARC. INF. NAO BATE C/ QTD PARC GER';
      048: Result := '048-CARNE COM PARCELAS COM ERRO';
      049: Result := '049-SEU NUMERO NAO CONFERE COM O CARNE';
      051: Result := '051-TITULO NAO ENCONTRADO';
      052: Result := '052-OCOR.  NAO ACATADA, TITULO  LIQUIDADO';
      053: Result := '053-OCOR. NAO ACATADA, TITULO BAIXADO';
      054: Result := '054-TITULO COM ORDEM DE PROTESTO JA EMITIDA';
      055: Result := '055-OCOR. NAO ACATADA, TITULO JA PROTESTADO';
      056: Result := '056-OCOR. NAO ACATADA, TIT. NAO VENCIDO';
      057: Result := '057-CEP DO SACADO INCORRETO';
      058: Result := '058-CGC/CPF INCORRETO';
      059: Result := '059-INSTRUCAO ACEITA SO P/ COBRANCA SIMPLES';
      060: Result := '060-ESPECIE DOCUMENTO NAO PROTESTAVEL';
      061: Result := '061-CEDENTE SEM CARTA DE PROTESTO';
      062: Result := '062-SACADO NAO PROTESTAVEL';
      063: Result := '063-CEP NAO ENCONTRADO NA TABELA DE PRACAS';
      064: Result := '064-TIPO DE COBRANCA NAO PERMITE PROTESTO';
      065: Result := '065-PEDIDO SUSTACAO JA SOLICITADO';
      066: Result := '066-SUSTACAO PROTESTO FORA DE PRAZO';
      067: Result := '067-CLIENTE NAO TRANSMITE REG. DE OCORRENCIA';
      068: Result := '068-TIPO DE VENCIMENTO INVALIDO';
      069: Result := '069-PRODUTO DIFERENTE DE COBRANCA SIMPLES';
      070: Result := '070-DATA PRORROGACAO MENOR QUE DATA VENCTO';
      071: Result := '071-DATA ANTECIPACAO MAIOR QUE DATA VENCTO';
      072: Result := '072-DATA DOCUMENTO SUPERIOR A DATA INSTRUCAO';
      073: Result := '073-ABATIMENTO MAIOR/IGUAL AO VALOR TITULO';
      074: Result := '074-PRIM. DESCONTO MAIOR/IGUAL VALOR TITULO';
      075: Result := '075-SEG. DESCONTO MAIOR/IGUAL VALOR TITULO';
      076: Result := '076-TERC. DESCONTO MAIOR/IGUAL VALOR TITULO';
      077: Result := '077-DESC. POR ANTEC. MAIOR/IGUAL VLR TITULO';
      078: Result := '078-NAO EXISTE ABATIMENTO P/ CANCELAR';
      079: Result := '079-NAO EXISTE PRIM. DESCONTO P/ CANCELAR';
      080: Result := '080-NAO EXISTE SEG. DESCONTO P/ CANCELAR';
      081: Result := '081-NAO EXISTE TERC. DESCONTO P/ CANCELAR';
      082: Result := '082-NAO EXISTE DESC. POR ANTEC. P/ CANCELAR';
      084: Result := '084-JA EXISTE SEGUNDO DESCONTO';
      085: Result := '085-JA EXISTE TERCEIRO DESCONTO';
      086: Result := '086-DATA SEGUNDO DESCONTO INVALIDA';
      087: Result := '087-DATA TERCEIRO DESCONTO INVALIDA';
      089: Result := '089-DATA MULTA MENOR/IGUAL QUE VENCIMENTO';
      090: Result := '090-JA EXISTE DESCONTO POR DIA ANTECIPACAO';
      091: Result := '091-JA EXISTE CONCESSAO DE DESCONTO';
      092: Result := '092-NOSSO NUMERO JA CADASTRADO';
      093: Result := '093-VALOR DO TITULO NAO INFORMADO';
      094: Result := '094-VALOR TIT. EM OUTRA MOEDA NAO INFORMADO';
      095: Result := '095-PERFIL NAO ACEITA VALOR TITULO ZERADO';
      096: Result := '096-ESPECIE DOCTO NAO PERMITE PROTESTO';
      097: Result := '097-ESPECIE DOCTO NAO PERMITE IOC ZERADO';
      098: Result := '098-DATA EMISSAO INVALIDA';
      099: Result := '099-REGISTRO DUPLICADO NO MOVIMENTO DIÁRIO';
      100: Result := '100-DATA EMISSAO MAIOR QUE A DATA VENCIMENTO';
      101: Result := '101-NOME DO SACADO NÃO INFORMADO';
      102: Result := '102-ENDERECO DO SACADO NÃO INFORMADO';
      103: Result := '103-MUNICIPIO DO SACADO NAO INFORMADO';
      104: Result := '104-UNIDADE DA FEDERACAO NAO INFORMADA';
      105: Result := '105-TIPO INSCRICAO NÃO EXISTE';
      106: Result := '106-CGC/CPF NAO INFORMADO';
      107: Result := '107-UNIDADE DA FEDERACAO INCORRETA';
      108: Result := '108-DIGITO CGC/CPF INCORRETO';
      109: Result := '109-VALOR MORA TEM QUE SER ZERO (TIT = ZERO)';
      110: Result := '110-DATA PRIMEIRO DESCONTO INVALIDA';
      111: Result := '111-DATA  DESCONTO NAO NUMERICA';
      112: Result := '112-VALOR DESCONTO NAO INFORMADO';
      113: Result := '113-VALOR DESCONTO INVALIDO';
      114: Result := '114-VALOR ABATIMENTO NAO INFORMADO';
      115: Result := '115-VALOR ABATIMENTO MAIOR VALOR TITULO';
      116: Result := '116-DATA MULTA NAO NUMERICA';
      117: Result := '117-VALOR DESCONTO MAIOR VALOR TITULO';
      118: Result := '118-DATA MULTA NAO INFORMADA';
      119: Result := '119-DATA MULTA MAIOR QUE DATA DE VENCIMENTO';
      120: Result := '120-PERCENTUAL MULTA NAO NUMERICO';
      121: Result := '121-PERCENTUAL MULTA NAO INFORMADO';
      122: Result := '122-VALOR IOF MAIOR QUE VALOR TITULO';
      123: Result := '123-CEP DO SACADO NAO NUMERICO';
      124: Result := '124-CEP SACADO NAO ENCONTRADO';
      126: Result := '126-CODIGO P. BAIXA / DEVOL. INVALIDO';
      127: Result := '127-CODIGO P. BAIXA / DEVOL. NAO NUMERICA';
      128: Result := '128-CODIGO PROTESTO INVALIDO';
      129: Result := '129-ESPEC DE DOCUMENTO NAO NUMERICA';
      130: Result := '130-FORMA DE CADASTRAMENTO NAO NUMERICA';
      131: Result := '131-FORMA DE CADASTRAMENTO INVALIDA';
      132: Result := '132-FORMA CADAST. 2 INVALIDA PARA CARTEIRA 3';
      133: Result := '133-FORMA CADAST. 2 INVALIDA PARA CARTEIRA 4';
      134: Result := '134-CODIGO DO MOV. REMESSA NAO NUMERICO';
      135: Result := '135-CODIGO DO MOV. REMESSA INVALIDO';
      136: Result := '136-CODIGO BCO NA COMPENSACAO NAO NUMERICO';
      138: Result := '138-NUM. LOTE REMESSA(DETALHE) NAO NUMERICO';
      140: Result := '140-COD. SEQUEC.DO REG. DETALHE INVALIDO';
      141: Result := '141-NUM. SEQ. REG. DO LOTE NAO NUMERICO';
      142: Result := '142-NUM.AG.CEDENTE/DIG.NAO NUMERICO';
      144: Result := '144-TIPO DE DOCUMENTO NAO NUMERICO';
      145: Result := '145-TIPO DE DOCUMENTO INVALIDO';
      146: Result := '146-CODIGO P. PROTESTO NAO NUMERICO';
      147: Result := '147-QTDE DE DIAS P. PROTESTO INVALIDO';
      148: Result := '148-QTDE DE DIAS P. PROTESTO NAO NUMERICO';
      149: Result := '149-CODIGO DE MORA INVALIDO';
      150: Result := '150-CODIGO DE MORA NAO NUMERICO';
      151: Result := '151-VL.MORA IGUAL A ZEROS P. COD.MORA 1';
      152: Result := '152-VL. TAXA MORA IGUAL A ZEROS P.COD MORA 2';
      154: Result := '154-VL. MORA NAO NUMERICO P. COD MORA 2';
      155: Result := '155-VL. MORA INVALIDO P. COD.MORA 4';
      156: Result := '156-QTDE DIAS P.BAIXA/DEVOL. NAO NUMERICO';
      157: Result := '157-QTDE DIAS BAIXA/DEV. INVALIDO P. COD. 1';
      158: Result := '158-QTDE DIAS BAIXA/DEV. INVALIDO P.COD. 2';
      160: Result := '160-BAIRRO DO SACADO NAO INFORMADO';
      161: Result := '161-TIPO INSC.CPF/CGC SACADOR/AVAL.NAO NUM.';
      162: Result := '162-INDICADOR DE CARNE NAO NUMERICO';
      163: Result := '163-NUM. TOTAL DE PARC.CARNE NAO NUMERICO';
      164: Result := '164-NUMERO DO PLANO NAO NUMERICO';
      165: Result := '165-INDICADOR DE PARCELAS CARNE INVALIDO';
      166: Result := '166-N.SEQ. PARCELA INV.P.INDIC. MAIOR 0';
      167: Result := '167-N. SEQ.PARCELA INV.P.INDIC.DIF.ZEROS';
      168: Result := '168-N.TOT.PARC.INV.P.INDIC. MAIOR ZEROS';
      169: Result := '169-NUM.TOT.PARC.INV.P.INDIC.DIFER.ZEROS';
      170: Result := '170-FORMA DE CADASTRAMENTO 2 INV.P.CART.5';
      199: Result := '199-TIPO INSC.CGC/CPF SACADOR.AVAL.INVAL.';
      200: Result := '200-NUM.INSC.(CGC)SACADOR/AVAL.NAO NUMERICO';
      201: Result := '201-ALT. DO CONTR. PARTICIPANTE INVALIDO';
      202: Result := '202-ALT. DO SEU NUMERO INVALIDA';
      218: Result := '218-BCO COMPENSACAO NAO NUMERICO (D3Q)';
      219: Result := '219-BCO COMPENSACAO INVALIDO (D3Q)';
      220: Result := '220-NUM. DO LOTE REMESSA NAO NUMERICO(D3Q)';
      221: Result := '221-NUM. SEQ. REG. NO LOTE (D3Q)';
      222: Result := '222-TIPO INSC.SACADO NAO NUMERICO (D3Q)';
      223: Result := '223-TIPO INSC.SACADO INVALIDO (D3Q)';
      224: Result := '224-NUM.INSC.SACADO NAO NUMERICO (D3Q)';
      225: Result := '225-NUM.INSC.SAC.INV.P.TIPO INSC.0 E 9(D3Q)';
      226: Result := '226-NUM.BCO COMPENSACAO NAO NUMERICO (D3R)';
      228: Result := '228-NUM. LOTE REMESSA NAO NUMERICO (D3R)';
      229: Result := '229-NUM. SEQ. REG. LOTE NAO NUMERICO (D3R)';
      246: Result := '246-COD.BCO COMPENSACAO NAO NUMERICO (D3S)';
      247: Result := '247-COD. BANCO COMPENSACAO INVALIDO (D3S)';
      248: Result := '248-NUM.LOTE REMESSA NAO NUMERICO (D3S)';
      249: Result := '249-NUM.SEQ.DO REG.LOTE NAO NUMERICO (D3S)';
      250: Result := '250-NUM.IDENT.DE IMPRESSAO NAO NUMERICO(D3S)';
      251: Result := '251-NUM.IDENT.DE IMPRESSAO INVALIDO (D3S)';
      252: Result := '252-NUM.LINHA IMPRESSA NAO NUMERICO(D3S)';
      253: Result := '253-COD.MSG. P.REC. SAC. NAO NUMERICO (D3S)';
      254: Result := '254-COD.MSG.P.REC.SACADO INVALIDO(D3S)';
      258: Result := '258-VL.MORA NAO NUMERICO P.COD=4(D3P)';
      259: Result := '259-CAD.TXPERM.SK.INV.P.COD.MORA=4(D3P)';
      260: Result := '260-VL.TIT(REAL).INV.P.COD.MORA = 1(DEP)';
      261: Result := '261-VL.OUTROS INV.P.COD.MORA = 1(D3P)';
    else
      Result := IntToStrZero(CodMotivo, 3) + ' - Outros Motivos';
    end;
  end
  else // 240
  begin
    case TipoOcorrencia of
    toRetornoComandoRecusado: //03 (Entrada rejeitada)
      case CodMotivo of
        01: Result:='Codigo do banco invalido';
        02: Result:='Codigo do registro detalhe invalido';
        03: Result:='Codigo do segmento invalido';
        04: Result:='Codigo do movimento nao permitido para carteira';
        05: Result:='Codigo de movimento invalido';
        06: Result:='Tipo/numero de inscricao do beneficiário invalidos';
        07: Result:='Agencia/Conta/DV invalido';
        08: Result:='Nosso numero invalido';
        09: Result:='Nosso numero duplicado';
        10: Result:='Carteira invalida';
        11: Result:='Forma de cadastramento do titulo invalido';
        12: Result:='Tipo de documento invalido';
        13: Result:='Identificacao da emissao do bloqueto invalida';
        14: Result:='Identificacao da distribuicao do bloqueto invalida';
        15: Result:='Caracteristicas da cobranca incompativeis';
        16: Result:='Data de vencimento invalida';
        17: Result:='Data de vencimento anterior a data de emissao';
        18: Result:='Vencimento fora do prazo de operacao';
        19: Result:='Titulo a cargo de Bancos Correspondentes com vencimento inferior XX dias';
        20: Result:='Valor do titulo invalido';
        21: Result:='Especie do titulo invalida';
        22: Result:='Especie nao permitida para a carteira';
        23: Result:='Aceite invalido';
        24: Result:='Data da emissao invalida';
        25: Result:='Data da emissao posterior a data';
        26: Result:='Codigo de juros de mora invalido';
        27: Result:='Valor/Taxa de juros de mora invalido';
        28: Result:='Codigo do desconto invalido';
        29: Result:='Valor do desconto maior ou igual ao valor do titulo ';
        30: Result:='Desconto a conceder nao confere';
        31: Result:='Concessao de desconto - ja existe desconto anterior';
        32: Result:='Valor do IOF invalido';
        33: Result:='Valor do abatimento invalido';
        34: Result:='Valor do abatimento maior ou igual ao valor do titulo';
        35: Result:='Abatimento a conceder nao confere';
        36: Result:='Concessao de abatimento - ja existe abatimento anterior';
        37: Result:='Codigo para protesto invalido';
        38: Result:='Prazo para protesto invalido';
        39: Result:='Pedido de protesto nao permitido para o titulo';
        40: Result:='Titulo com ordem de protesto emitida';
        41: Result:='Pedido de cancelamento/sustacao para titulos sem instrucao de protesto';
        42: Result:='Codigo para baixa/devolucao invalido';
        43: Result:='Prazo para baixa/devolucao invalido';
        44: Result:='Codigo da moeda invalido';
        45: Result:='Nome do pagador nao informado';
        46: Result:='Tipo/numero de inscricao do pagador invalidos';
        47: Result:='Endereco do pagador nao informado';
        48: Result:='CEP invalido';
        49: Result:='CEP sem praca de cobranca /nao localizado';
        50: Result:='CEP referente a um Banco Correspondente';
        51: Result:='CEP incompativel com a unidade da federacao';
        52: Result:='Unidade da federacao invalida';
        53: Result:='Tipo/numero de inscricao do sacador/avalista invalidos';
        54: Result:='Sacador/Avalista nao informado';
        55: Result:='Nosso numero no Banco Correspondente nao informado';
        56: Result:='Codigo do Banco Correspondente nao informado';
        57: Result:='Codigo da multa invalido';
        58: Result:='Data da multa invalida';
        59: Result:='Valor/Percentual da multa invalido';
        60: Result:='Movimento para titulo nao cadastrado';
        61: Result:='Alteracao da agencia cobradora/dv invalida';
        62: Result:='Tipo de impressao invalido';
        63: Result:='Entrada para titulo ja cadastrado';
        64: Result:='Numero da linha invalido';
        65: Result:='A espécie de título não permite a instrução';
        72: Result:='Entrada de título Sem Registro';
        90: Result:='Identificador/Quantidade de Parcelas de carnê invalido';
        92: Result:='Data de Desconto Inválida';
      end;
    toRetornoLiquidadoSemRegistro, toRetornoLiquidado, toRetornoLiquidadoPorConta,
       toRetornoLiquidadoSaldoRestante, toRetornoLiquidadoEmCartorio: // 05, 06, 07, 08 e 15 (Liquidado)
      case CodMotivo of
        01: Result:='01-Por saldo';
        02: Result:='02-Por conta';
        03: Result:='03-No próprio banco';
        04: Result:='04-Compensação eletrônica';
        05: Result:='05-Compensação convencional';
        06: Result:='06-Arquivo magnético';
        07: Result:='07-Após feriado local';
        08: Result:='08-Em cartório';
        09: Result:='09-Pagamento Parcial';
      end;
    else
      Result := IntToStrZero(CodMotivo, 2) + ' - Outros Motivos';
    end; //case TipoOcorrencia

  end; //else 240

end;


end.


