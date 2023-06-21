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

unit ACBrBancoItau;

interface

uses
  Classes, SysUtils, Contnrs,
  ACBrBoleto, ACBrBoletoConversao;

type
  { TACBrBancoItau}

  TACBrBancoItau = class(TACBrBancoClass)
  private
    fTipoOcorrenciaRemessa : String;
    fDataProtestoNegativacao : TDateTime;
    fDiasProtestoNegativacao : String;
  protected
    function DefineNumeroDocumentoModulo(const ACBrTitulo: TACBrTitulo): String; override;
    function DefineCampoLivreCodigoBarras(const ACBrTitulo: TACBrTitulo): String; override;
    function DefineCampoConvenio(const ATam: Integer = 20): String; override;
    function DefineCampoDigitoAgencia: String; override;
    function DefineCampoDigitoConta: String; override;
    function DefineCampoDigitoAgenciaConta: String; override;
    function DefinePosicaoUsoExclusivo: String; override;
    function DefineEspecieDoc(const ACBrTitulo: TACBrTitulo): String; override;
    function DefineTipoSacado(const ACBrTitulo: TACBrTitulo): String; override;
    function DefinePosicaoNossoNumeroRetorno: Integer; override;
    function DefinePosicaoCarteiraRetorno:Integer; override;
    function InstrucoesProtesto(const ACBrTitulo: TACBrTitulo): String;override;
    function MontaInstrucoesCNAB400(const ACBrTitulo :TACBrTitulo; const nRegistro: Integer ): String; override;

    function ConverteEspecieDoc(const ACodigoEspecie: Integer = 0): String;
    procedure DefineDataProtestoNegativacao(const ACBrTitulo: TACBrTitulo);
    procedure EhObrigatorioAgenciaDV; override;
  public
    Constructor create(AOwner: TACBrBanco);
    function MontarCampoNossoNumero ( const ACBrTitulo: TACBrTitulo) : String; override;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String; override;

    function GerarRegistroTransacao240(ACBrTitulo : TACBrTitulo): String; override;
    function GerarRegistroTrailler240(ARemessa : TStringList): String;  override;
    procedure GerarRegistroHeader400(NumeroRemessa : Integer; aRemessa: TStringList); override;
    procedure GerarRegistroTransacao400(ACBrTitulo : TACBrTitulo; aRemessa: TStringList); override;

    Procedure LerRetorno400(ARetorno:TStringList); override;

    function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia) : String; override;
    function CodOcorrenciaToTipo(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
    function TipoOCorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia):String; override;
    function CodMotivoRejeicaoToDescricao(const TipoOcorrencia:TACBrTipoOcorrencia; CodMotivo:Integer): String; override;

    function CodOcorrenciaToTipoRemessa(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
    function CodigoLiquidacao_Descricao( CodLiquidacao : String) : String;

    function TipoOcorrenciaToCodRemessa(const TipoOcorrencia: TACBrTipoOcorrencia): String; override;

    property TipoOcorrenciaRemessa: String read fTipoOcorrenciaRemessa write fTipoOcorrenciaRemessa;
    property DataProtestoNegativacao : TDateTime read  fDataProtestoNegativacao ;
    property DiasProtestoNegativacao : String read fDiasProtestoNegativacao ;

  end;

implementation

uses
  {$IFDEF COMPILER6_UP} dateutils {$ELSE} ACBrD5 {$ENDIF},
  StrUtils, Variants, ACBrValidador, ACBrUtil.Base, ACBrUtil.FilesIO,
  ACBrUtil.Strings, ACBrUtil.DateTime;

constructor TACBrBancoItau.create(AOwner: TACBrBanco);
begin
   inherited create(AOwner);
   fpDigito                := 7;
   fpNome                  := 'BANCO ITAU SA';
   fpNumero                := 341;
   fpTamanhoMaximoNossoNum := 8;
   fpTamanhoAgencia        := 4;
   fpTamanhoConta          := 5;
   fpTamanhoCarteira       := 3;
   fpTamanhoNumeroDocumento:= 10;
   fTipoOcorrenciaRemessa  := '01';
   fpQtdRegsLote           := 0;
   fpQtdRegsCobranca       := 0;
   fpVlrRegsCobranca       := 0;
   fpLayoutVersaoArquivo   := 40;
   fpLayoutVersaoLote      := 30;
   fpDensidadeGravacao     := '0';
   fpModuloMultiplicadorInicial:= 1;
   fpModuloMultiplicadorFinal:= 2;
   fpModuloMultiplicadorAtual:= 2;
   fpCodigosMoraAceitos    := '1235';
end;

function TACBrBancoItau.DefineNumeroDocumentoModulo(
  const ACBrTitulo: TACBrTitulo): String;
var
  Docto: String;
begin
  Result := '0';
  Docto := '';

  with ACBrTitulo do
  begin
     if MatchText( Carteira , ['116','117','119','134','135','136','104',
     '147','105','112','212','166','113','126','131','145','150','168']) then
           Docto := Carteira + PadLeft(NossoNumero,TamanhoMaximoNossoNum,'0')
        else
           Docto := ACBrBoleto.Cedente.Agencia + ACBrBoleto.Cedente.Conta +
                    Carteira + PadLeft(ACBrTitulo.NossoNumero,TamanhoMaximoNossoNum,'0')
  end;
  Modulo.FormulaDigito := frModulo10;  //Particularidade do Itau
  Result := Docto;

end;

function TACBrBancoItau.DefineCampoLivreCodigoBarras(
  const ACBrTitulo: TACBrTitulo): String;
var
  ANossoNumero, aAgenciaCC: String;
begin
  Result := '';
  with ACBrTitulo do
  begin
    ANossoNumero := ACBrTitulo.Carteira +
                    PadLeft(NossoNumero,8,'0') +
                    CalcularDigitoVerificador(ACBrTitulo);

    AAgenciaCC   := OnlyNumber(ACBrTitulo.ACBrBoleto.Cedente.Agencia +
                  ACBrTitulo.ACBrBoleto.Cedente.Conta   +
                  ACBrTitulo.ACBrBoleto.Cedente.ContaDigito);


    Result := ANossoNumero +
              AAgenciaCC +
              '000';
  end;

end;

function TACBrBancoItau.DefineCampoConvenio(const ATam: Integer): String;
begin
  Result := Space(ATam);
end;

function TACBrBancoItau.DefineCampoDigitoAgencia: String;
begin
  Result := Space(1);
end;

function TACBrBancoItau.DefineCampoDigitoConta: String;
begin
  Result := Space(1);
end;

function TACBrBancoItau.DefineCampoDigitoAgenciaConta: String;
begin
  Result := PadLeft(ACBrBanco.ACBrBoleto.Cedente.ContaDigito, 1, '0');
end;

function TACBrBancoItau.DefinePosicaoUsoExclusivo: String;
begin
  Result := space(54)                          + // 172 a 225 - 54 Brancos
            '000'                              + // 226 a 228 - zeros
            space(12);                           // 229 a 240 - Brancos
end;

function TACBrBancoItau.DefineEspecieDoc(const ACBrTitulo: TACBrTitulo): String;
begin
  with ACBrTitulo do
  begin
    if ACBrBanco.ACBrBoleto.LayoutRemessa = c240 then
    begin
       if AnsiSameText(EspecieDoc, 'DM') then
        Result := '01'
       else if AnsiSameText(EspecieDoc,'NP') then
        Result := '02'
       else if AnsiSameText(EspecieDoc,'NS') then
        Result := '03'
       else if AnsiSameText(EspecieDoc,'ME') then
        Result := '04'
       else if AnsiSameText(EspecieDoc,'RC') then
        Result := '05'
       else if AnsiSameText(EspecieDoc,'CTR') then
        Result := '06'
       else if AnsiSameText(EspecieDoc,'CSG') then
        Result := '07'
       else if AnsiSameText(EspecieDoc,'DS') then
        Result := '08'
       else if AnsiSameText(EspecieDoc,'LC') then
        Result := '09'
       else if AnsiSameText(EspecieDoc,'ND') then
        Result := '13'
       else if AnsiSameText(EspecieDoc,'DD') then
        Result := '15'
       else if AnsiSameText(EspecieDoc,'EC') then
        Result := '16'
       else if AnsiSameText(EspecieDoc,'CPS') then
        Result := '17'
       else if AnsiSameText(EspecieDoc,'BDP') then
        Result := '18'
       else
        Result := '99';
    end
    else
    begin
      if trim(EspecieDoc) = 'DM' then
        Result:= '01'
      else if trim(EspecieDoc) = 'NP' then
        Result:= '02'
      else if trim(EspecieDoc) = 'NS' then
        Result:= '03'
      else if trim(EspecieDoc) = 'ME' then
        Result:= '04'
      else if trim(EspecieDoc) = 'RC' then
        Result:= '05'
      else if trim(EspecieDoc) = 'CT' then
        Result:= '06'
      else if trim(EspecieDoc) = 'CS' then
        Result:= '07'
      else if trim(EspecieDoc) = 'DS' then
        Result:= '08'
      else if trim(EspecieDoc) = 'LC' then
        Result:= '09'
      else if trim(EspecieDoc) = 'ND' then
        Result:= '13'
      else if trim(EspecieDoc) = 'DD' then
        Result:= '15'
      else if trim(EspecieDoc) = 'EC' then
        Result:= '16'
      else if trim(EspecieDoc) = 'PS' then
        Result:= '17'
      else
        Result:= '99';
    end;

  end;
end;

function TACBrBancoItau.DefineTipoSacado(const ACBrTitulo: TACBrTitulo): String;
begin
  with ACBrTitulo do
  begin
    case Sacado.Pessoa of
        pFisica   : Result := '1';
        pJuridica : Result := '2';
     else
        Result := '9';
     end;

  end;
end;

procedure TACBrBancoItau.EhObrigatorioAgenciaDV;
begin
  //sem validação
end;

function TACBrBancoItau.DefinePosicaoNossoNumeroRetorno: Integer;
begin
  if ACBrBanco.ACBrBoleto.LayoutRemessa = c240 then
    Result := 41
  else
    Result := 63;
end;

function TACBrBancoItau.DefinePosicaoCarteiraRetorno: Integer;
begin
  if ACBrBanco.ACBrBoleto.LayoutRemessa = c240 then
    Result := 38
  else
    Result := 83;
end;

function TACBrBancoItau.InstrucoesProtesto(const ACBrTitulo: TACBrTitulo): String;
begin
  with ACBrTitulo do
  begin
    if ((DataProtesto > 0) and (DataProtesto > Vencimento)) then
    begin
      case TipoDiasProtesto of
        diCorridos : Result := '34';
        diUteis    : Result := '35';
      else
        Result := '';
      end;
    end else
    begin
      if ((DataNegativacao > 0) and (DataNegativacao > Vencimento)) then
        Result := '66'
      else
        Result := '';
    end;
      if (PadLeft(trim(Instrucao1),2,'0') = '00') and (Result <> '') then
        Instrucao1:= Result;
  end;
end;

function TACBrBancoItau.MontaInstrucoesCNAB400(const ACBrTitulo: TACBrTitulo;
  const nRegistro: Integer): String;
begin
  Result := '';
  with ACBrTitulo, ACBrBoleto do
  begin
    {Nenhum mensagem especificada. Registro não será necessário gerar o registro}
    if Mensagem.Count = 0 then
       Exit;

    Result := sLineBreak + '6'                        +                       // IDENTIFICAÇÃO DO REGISTRO
              '2'                                     +                       // IDENTIFICAÇÃO DO LAYOUT PARA O REGISTRO
              Copy(PadRight(Mensagem[0], 69, ' '), 1, 69);                    // CONTEÚDO DA 1ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO

    if Mensagem.Count >= 2 then
       Result := Result + Copy(PadRight(Mensagem[1], 69, ' '), 1, 69)         // CONTEÚDO DA 2ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO
    else
       Result := Result + PadRight('', 69, ' ');                              // CONTEÚDO DO RESTANTE DAS LINHAS

    if Mensagem.Count >= 3 then
       Result := Result + Copy(PadRight(Mensagem[2], 69, ' '), 1, 69)         // CONTEÚDO DA 3ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO
    else
      Result := Result + PadRight('', 69, ' ');                               // CONTEÚDO DO RESTANTE DAS LINHAS

    if Mensagem.Count >= 4 then
       Result := Result + Copy(PadRight(Mensagem[3], 69, ' '), 1, 69)         // CONTEÚDO DA 4ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO
    else
       Result := Result + PadRight('', 69, ' ');                              // CONTEÚDO DO RESTANTE DAS LINHAS

    if Mensagem.Count >= 5 then
       Result := Result + Copy(PadRight(Mensagem[4], 69, ' '), 1, 69)         // CONTEÚDO DA 5ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO
    else
       Result := Result + PadRight('', 69, ' ');                              // CONTEÚDO DO RESTANTE DAS LINHAS

    Result := Result    +
              space(47) +                                                     // COMPLEMENTO DO REGISTRO
              IntToStrZero(nRegistro + 2, 6);                                 // Nº SEQÜENCIAL DO REGISTRO NO ARQUIVO
  end;
end;

function TACBrBancoItau.ConverteEspecieDoc(const ACodigoEspecie: Integer): String;
begin
  if (ACodigoEspecie = 0) then
    Result := '99'
  else
    case ACodigoEspecie of
      01 : Result := 'DM';
      02 : Result := 'NP';
      03 : Result := 'NS';
      04 : Result := 'ME';
      05 : Result := 'RC';
      06 : Result := 'CT';
      07 : Result := 'CS';
      08 : Result := 'DS';
      09 : Result := 'LC';
      13 : Result := 'ND';
      15 : Result := 'DD';
      16 : Result := 'EC';
      17 : Result := 'PS';
      99 : Result := 'DV';
    else
      Result := 'DV';
    end;

end;

procedure TACBrBancoItau.DefineDataProtestoNegativacao(
  const ACBrTitulo: TACBrTitulo);
var
  ACodProtesto: String;
begin

  with ACBrTitulo do
  begin
    ACodProtesto :=  DefineCodigoProtesto(ACBrTitulo);
    if ( ACodProtesto = '7') then
      begin
        fDataProtestoNegativacao := DataNegativacao;
        fDiasProtestoNegativacao := IntToStr(DiasDeNegativacao);
      end
      else
      begin
        if ((ACodProtesto <> '3') and (ACodProtesto <> '8')) then
        begin
          fDataProtestoNegativacao := DataProtesto;
          fDiasProtestoNegativacao := IntToStr(DiasDeProtesto);
        end
        else
        begin
          fDataProtestoNegativacao := 0;
          fDiasProtestoNegativacao := '0';
        end;
      end;

  end;
end;

function TACBrBancoItau.MontarCampoNossoNumero ( const ACBrTitulo: TACBrTitulo
   ) : String;
var
  NossoNr: String;
begin
  with ACBrTitulo do
  begin
    NossoNr := Carteira + PadLeft(NossoNumero,TamanhoMaximoNossoNum,'0');
  end;

  Insert('/',NossoNr,4);  Insert('-',NossoNr,13);
  Result := NossoNr + CalcularDigitoVerificador(ACBrTitulo);
end;

function TACBrBancoItau.MontarCampoCodigoCedente (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
   Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia  +'/'+
             ACBrTitulo.ACBrBoleto.Cedente.Conta    +'-'+
             ACBrTitulo.ACBrBoleto.Cedente.ContaDigito;
end;

function TACBrBancoItau.GerarRegistroTransacao240(ACBrTitulo : TACBrTitulo): String;
var
   ATipoInscricao,
   AEspecieDoc,
   ADataMoraJuros,
   ADataDesconto,
   ATipoAceite,
   ACodigoNegativacao,
   ATipoInscricaoAvalista: String;
   ListTransacao: TStringList;
begin
  ListTransacao:= TStringList.Create;
  try
    with ACBrTitulo do
    begin
       {SEGMENTO P}
       inc(fpQtdRegsLote);
       {Tipo de Ocorrencia}
       TipoOcorrenciaRemessa := TipoOcorrenciaToCodRemessa(ACBrTitulo.OcorrenciaOriginal.Tipo);

       {Especie Documento}
       AEspecieDoc := DefineEspecieDoc(ACBrTitulo);

       {Aceite do Titulo }
       ATipoAceite := DefineAceite(ACBrTitulo);

       {Código de Protesto/Negativação }
       ACodigoNegativacao := DefineCodigoProtesto(ACBrTitulo);
       DefineDataProtestoNegativacao(ACBrTitulo);

       {Data Mora}
       ADataMoraJuros := DefineDataMoraJuros(ACBrTitulo);

       {Data Desconto}
       ADataDesconto := DefineDataDesconto(ACBrTitulo);

       {Pegando tipo de pessoa do Sacado}
       ATipoInscricao := DefineTipoSacado(ACBrTitulo);

       {Pegando tipo de pessoa do Avalista}
       ATipoInscricaoAvalista:= DefineTipoSacadoAvalista(ACBrTitulo);

       VlrRegsCobranca:= VlrRegsCobranca  + ValorDocumento;

       ListTransacao.Add( IntToStrZero(ACBrBanco.Numero, 3)                + //1 a 3 - Código do banco
                '0001'                                                     + //4 a 7 - Lote de serviço
                '3'                                                        + //8 - Tipo do registro: Registro detalhe
                IntToStrZero(fpQtdRegsLote ,5)                             + //9 a 13 - Número seqüencial do registro no lote - Cada registro possui dois segmentos
                'P'                                                        + //14 - Código do segmento do registro detalhe
                ' '                                                        + //15 - Uso exclusivo FEBRABAN/CNAB: Branco
                TipoOcorrenciaRemessa                                      + //16 a 17 - Código de movimento
                '0'                                                        + // 18
                PadLeft(OnlyNumber(ACBrBoleto.Cedente.Agencia),4,'0')      + //19 a 22 - Agência mantenedora da conta
                ' '                                                        + // 23
                '0000000'                                                  + //24 a 30 - Complemento de Registro
                PadLeft(OnlyNumber(ACBrBoleto.Cedente.Conta),5,'0')        + //31 a 35 - Número da Conta Corrente
                ' '                                                        + // 36
                ACBrBoleto.Cedente.ContaDigito                             + //37 - Dígito verificador da agência / conta
                PadLeft(Carteira, 3, ' ')                                  + // 38 a 40 - Carteira
                PadLeft(NossoNumero, 8, '0')                               + // 41 a 48 - Nosso número - identificação do título no banco
                CalcularDigitoVerificador(ACBrTitulo)                      + // 49 - Dígito verificador da agência / conta preencher somente em cobrança sem registro
                space(8)                                                   + // 50 a 57 - Brancos
                PadRight('', 5, '0')                                       + // 58 a 62 - Complemento
                PadRight(NumeroDocumento, 10, ' ')                         + // 63 a 72 - Número que identifica o título na empresa [ Alterado conforme instruções da CSO Brasília ] {27-07-09}

                space(5)                                                   + // 73 a 77 - Brancos
                FormatDateTime('ddmmyyyy', Vencimento)                     + // 78 a 85 - Data de vencimento do título
                IntToStrZero( round( ValorDocumento * 100), 15)            + // 86 a 100 - Valor nominal do título
                '00000'                                                    + // 101 a 105 - Agência cobradora. // Ficando com Zeros o Itaú definirá a agência cobradora pelo CEP do sacado
                '0'                                                        + // 106 - Dígito da agência cobradora
                PadRight(AEspecieDoc,2)                                    + // 107 a 108 - Espécie do documento
                ATipoAceite                                                + // 109 - Identificação de título Aceito / Não aceito
                FormatDateTime('ddmmyyyy', DataDocumento)                  + // 110 a 117 - Data da emissão do documento
                '0'                                                        + // 118 - Zeros
                ADataMoraJuros                                             + //119 a 126 - Data a partir da qual serão cobrados juros
                IfThen(ValorMoraJuros > 0, IntToStrZero( round(ValorMoraJuros * 100), 15),
                 PadLeft('', 15, '0'))                                     + //127 a 141 - Valor de juros de mora por dia
                '0'                                                        + // 142 - Zeros
                ADataDesconto                                              + // 143 a 150 - Data limite para desconto
                IfThen(ValorDesconto > 0, IntToStrZero( round(ValorDesconto * 100), 15),
                PadLeft('', 15, '0'))                                      + //151 a 165 - Valor do desconto por dia
                IntToStrZero( round(ValorIOF * 100), 15)                   + //166 a 180 - Valor do IOF a ser recolhido
                IntToStrZero( round(ValorAbatimento * 100), 15)            + //181 a 195 - Valor do abatimento
                PadRight(SeuNumero, 25, ' ')                               + //196 a 220 - Identificação do título na empresa
                ACodigoNegativacao                                         + //221 - Código de protesto: Protestar em XX dias corridos
                IfThen((DataProtestoNegativacao <> 0) and
                       (DataProtestoNegativacao > Vencimento),
                        PadLeft(DiasProtestoNegativacao , 2, '0'), '00')   + //222 a 223 - Prazo para protesto
                IfThen((DataBaixa <> 0) and (DataBaixa > Vencimento), '1', '0')  + // 224 - Código de Baixa
                IfThen((DataBaixa <> 0) and (DataBaixa > Vencimento),
                        PadLeft(IntToStr(DaysBetween(DataBaixa, Vencimento)), 2, '0'), '00')  + // 225 A 226 - Dias para baixa
                '0000000000000 ');


       {SEGMENTO Q}
       inc(fpQtdRegsLote);
       ListTransacao.Add( IntToStrZero(ACBrBanco.Numero, 3)                + //Código do banco
                '0001'                                                     + //Número do lote
                '3'                                                        + //Tipo do registro: Registro detalhe
                IntToStrZero(fpQtdRegsLote ,5)                             + //Número seqüencial do registro no lote - Cada registro possui dois segmentos
                'Q'                                                        + //Código do segmento do registro detalhe
                ' '                                                        + //Uso exclusivo FEBRABAN/CNAB: Branco
                TipoOcorrenciaRemessa                                      + // 16 a 17
                         {Dados do sacado}
                ATipoInscricao                                             + // 18 a 18 Tipo inscricao
                PadLeft(OnlyNumber(Sacado.CNPJCPF), 15, '0')               + // 19 a 33
                PadRight(Sacado.NomeSacado, 30, ' ')                       + // 34 a 63
                space(10)                                                  + // 64 a 73
                PadRight(Sacado.Logradouro +' '+ Sacado.Numero +' '+ Sacado.Complemento , 40, ' ') + // 74 a 113
                PadRight(Sacado.Bairro, 15, ' ')                           +  // 114 a 128
                PadLeft(Sacado.CEP, 8, '0')                                +  // 129 a 136
                PadRight(Sacado.Cidade, 15, ' ')                           +  // 137 a 151
                PadRight(Sacado.UF, 2, ' ')                                +  // 152 a 153
                         {Dados do sacador/avalista}
                ATipoInscricaoAvalista                                     + //Tipo de inscrição: Não informado
                PadLeft(Sacado.SacadoAvalista.CNPJCPF, 15, '0')            + //Número de inscrição
                PadRight(Sacado.SacadoAvalista.NomeAvalista, 30, ' ')      + //Nome do sacador/avalista
                space(10)                                                  + //Uso exclusivo FEBRABAN/CNAB
                PadRight('0',3, '0')                                       + //Uso exclusivo FEBRABAN/CNAB
                space(28));                                                   //Uso exclusivo FEBRABAN/CNAB



       {Segmento R}
       if(MatchText(TipoOcorrenciaRemessa,['01','49','31']))then
       begin
         inc(fpQtdRegsLote);
         ListTransacao.Add(IntToStrZero(ACBrBanco.Numero,3)                         + // 001 a 003 - Codigo do Banco
                '0001'                                                              + // 004 a 007 - Lote de Serviço
                '3'                                                                 + // 008 a 008 - Registro Detalhe
                IntToStrZero(fpQtdRegsLote ,5)                                      + // 009 a 013 - Seq. Registro do Lote
                'R'                                                                 + // 014 a 014 - Codigo do Segmento registro detalhe
                ' '                                                                 + // 015 a 015 - Complemento de Registro
                TipoOcorrenciaRemessa                                               + // 016 a 017 - Identificação da Ocorrencia
                '0'                                                                 + // 018 a 018 - Complemento de Registro

                IfThen(ValorDesconto2>0,FormatDateTime( 'ddmmyyyy', DataDesconto2),
                StringOfChar('0',8))                                                + // 019 a 026 - Data Segundo Desconto

                IntToStrZero(round(ValorDesconto2 * 100),15)                        + // 027 a 041 - Valor Segundo Desconto
                '0'                                                                 + // 042 a 042 - Complemento de Registro

                IfThen(ValorDesconto3>0,FormatDateTime( 'ddmmyyyy', DataDesconto3),
                StringOfChar('0',8))                                                + // 043 a 050 - Data Terceiro Desconto

                IntToStrZero(round(ValorDesconto3 * 100),15)                        + // 051 a 065 - Valor Terceiro Desconto

                IfThen((PercentualMulta > 0),
                      IfThen(MultaValorFixo,'1','2'), '0')                          + // 066 a 066 1- Cobrar Multa Valor Fixo / 2- Percentual / 0-Não cobrar multa
                IfThen((PercentualMulta > 0),
                       FormatDateTime('ddmmyyyy', DataMulta), '00000000')           + // 067 a 074 Se cobrar informe a data para iniciar a cobrança ou informe zeros se não cobrar
                IfThen( (PercentualMulta > 0), IntToStrZero(round(PercentualMulta * 100), 15),
                         PadRight('', 15, '0'))                                     + // 075 a 089 Valor / Percentual de multa.

                StringOfChar(' ',10)                                                + // 090 a 099 Complemento de Registro
                StringOfChar(' ',40)                                                + // 100 a 139 Informação ao Pagador
                StringOfChar(' ',60)                                                + // 140 a 199 Complemento de Registro
                '00000000'                                                          + // 200 a 207 Codigo de Ocorrencia do Pagador
                '00000000'                                                          + // 208 a 215 Complemento de Registro
                ' '                                                                 + // 216 a 216 Complemento de Registro
                StringOfChar('0',12)                                                + // 217 a 228 Complemento de Registro
                '  '                                                                + // 229 a 230 Complemento de Registro
                '0'                                                                 + // 231 a 231 Complemento de Registro
                StringOfChar(' ',9));                                                  // 232 a 240 Complemento de Registro

       end;

    end;
    Result := RemoverQuebraLinhaFinal(ListTransacao.Text);
  finally
    ListTransacao.Free;
  end;
end;

function TACBrBancoItau.GerarRegistroTrailler240(ARemessa: TStringList): String;
begin
  fpQtdRegsCobranca:= fpQtdRegsLote;
  Result:= inherited GerarRegistroTrailler240(ARemessa);
  fpQtdRegsLote := 0;
end;

procedure TACBrBancoItau.GerarRegistroHeader400(
  NumeroRemessa: Integer; aRemessa: TStringList);
var
   wLinha: String;
begin
   with ACBrBanco.ACBrBoleto.Cedente do
   begin

      { GERAR REGISTRO-HEADER DO ARQUIVO }
      wLinha:=    '0'                                  + // 1 a 1     - IDENTIFICAÇÃO DO REGISTRO HEADER
                  '1'                                  + // 2 a 2     - TIPO DE OPERAÇÃO - REMESSA
                  'REMESSA'                            + // 3 a 9     - IDENTIFICAÇÃO POR EXTENSO DO MOVIMENTO
                  '01'                                 + // 10 a 11   - IDENTIFICAÇÃO DO TIPO DE SERVIÇO
                  PadRight('COBRANCA',15, ' ')         + // 12 a 26   - IDENTIFICAÇÃO POR EXTENSO DO TIPO DE SERVIÇO
                  PadLeft(OnlyNumber(Agencia), 4, '0') + // 27 a 30   - AGÊNCIA MANTENEDORA DA CONTA
                  '00'                                 + // 31 a 32   - COMPLEMENTO DE REGISTRO
                  PadLeft(Conta, 5, '0')               + // 33 a 37   - NÚMERO DA CONTA CORRENTE DA EMPRESA
                  PadLeft(ContaDigito, 1, '0')         + // 38 a 38   - DÍGITO DE AUTO CONFERÊNCIA AG/CONTA EMPRESA
                  space(8)                             + // 39 a 46   - COMPLEMENTO DO REGISTRO
                  PadRight(Nome, 30, ' ')              + // 47 a 76   - NOME POR EXTENSO DA "EMPRESA MÃE"
                  IntToStrZero(ACBrBanco.Numero, 3)    + // 77 a 79   - Nº DO BANCO NA CÂMARA DE COMPENSAÇÃO
                  PadRight(fpNome, 15, ' ')            + // 80 a 94   - NOME POR EXTENSO DO BANCO COBRADOR
                  FormatDateTime('ddmmyy', Now)        + // 95 a 100  - DATA DE GERAÇÃO DO ARQUIVO
                  space(294)                           + // 101 a 394 - COMPLEMENTO DO REGISTRO
                  IntToStrZero(1,6);                     // 395 a 400 - NÚMERO SEQÜENCIAL DO REGISTRO NO ARQUIVO
      aRemessa.Add(UpperCase(wLinha));
   end;
end;

procedure TACBrBancoItau.GerarRegistroTransacao400( ACBrTitulo: TACBrTitulo; aRemessa: TStringList);
var
   ATipoCedente, ATipoSacado, ATipoSacadoAvalista, ATipoOcorrencia    :String;
   ADataMoraJuros, ADataDesconto, ATipoAceite    :String;
   ATipoEspecieDoc, ANossoNumero,wLinha,wCarteira :String;
   wLinhaMulta :String;
   iSequencia : integer;

begin
   with ACBrTitulo do
   begin
     {Tipo de Ocorrencia}
     ATipoOcorrencia := TipoOcorrenciaToCodRemessa(ACBrTitulo.OcorrenciaOriginal.Tipo);

     {Aceite do Titulo }
     ATipoAceite := DefineAceite(ACBrTitulo);

     {Especie Documento}
     ATipoEspecieDoc := DefineEspecieDoc(ACBrTitulo);

     {Data Mora}
     ADataMoraJuros := DefineDataMoraJuros(ACBrTitulo, 'ddmmyy');

     {Descontos}
     ADataDesconto := DefineDataDesconto(ACBrTitulo, 'ddmmyy');

     {Pegando Tipo de Cedente}
     ATipoCedente := DefineTipoInscricao;

     {Pegando Tipo de Sacado}
     ATipoSacado:= DefineTipoSacado(ACBrTitulo);

     {Pegando Tipo de Sacado Avalista}
     ATipoSacadoAvalista := DefineTipoSacadoAvalista(ACBrTitulo);

     {Pegando campo Intruções conforme código protesto}
     InstrucoesProtesto(ACBrTitulo);
     DefineDataProtestoNegativacao(ACBrTitulo);

    with ACBrBoleto do
    begin
       wCarteira:= Trim(Carteira);
       {Cobrança sem registro com opção de envio de arquivo remessa}
       if (wCarteira = '102') or (wCarteira = '103') or
          (wCarteira = '107') or (wCarteira = '172') or
          (wCarteira = '173') or (wCarteira = '196') then
        begin
          ANossoNumero := MontarCampoNossoNumero(ACBrTitulo);
          wLinha:= '6'                                                                            + // 6 - FIXO
                   '1'                                                                            + // 1 - FIXO
                   PadLeft(OnlyNumber(Cedente.Agencia), 4, '0')                                   + // AGÊNCIA MANTENEDORA DA CONTA
                   '00'                                                                           + // COMPLEMENTO DE REGISTRO
                   PadLeft(OnlyNumber(Cedente.Conta), 5, '0')                                     + // NÚMERO DA CONTA CORRENTE DA EMPRESA
                   PadRight(Cedente.ContaDigito, 1)                                               + // DÍGITO DE AUTO CONFERÊNCIA AG/CONTA EMPRESA
                   PadLeft(Carteira,3,' ')                                                        + // NÚMERO DA CARTEIRA NO BANCO
                   PadLeft(NossoNumero, 8, '0')                                                   + // IDENTIFICAÇÃO DO TÍTULO NO BANCO
                   Copy(ANossoNumero, Length(ANossoNumero), 1)                                    + // DAC DO NOSSO NÚMERO
                   '0'                                                                            + // 0 - R$
                   PadRight('R$', 4, ' ')                                                         + // LITERAL DE MOEDA
                   IntToStrZero( round( ValorDocumento * 100), 13)                                + // VALOR NOMINAL DO TÍTULO
                   PadRight(SeuNumero, 10, ' ')                                                   + // IDENTIFICAÇÃO DO TÍTULO NA EMPRESA
                   FormatDateTime('ddmmyy', Vencimento)                                           + // DATA DE VENCIMENTO DO TÍTULO
                   PadLeft(ATipoEspecieDoc, 2, '0')                                               + // ESPÉCIE DO TÍTULO
                   ATipoAceite                                                                    + // IDENTIFICAÇÃO DE TITILO ACEITO OU NÃO ACEITO
                   FormatDateTime('ddmmyy', DataDocumento)                                        + // DATA DE EMISSÃO
                   {Dados do sacado}
                   PadLeft(ATipoSacado, 2, '0')                                                   + // IDENTIFICAÇÃO DO TIPO DE INSCRIÇÃO/SACADO
                   PadLeft(OnlyNumber(Sacado.CNPJCPF), 15, '0')                                   + // Nº DE INSCRIÇÃO DO SACADO  (CPF/CGC)
                   PadRight(Sacado.NomeSacado, 30, ' ')                                           + // NOME DO SACADO
                   space(9)                                                                       + // BRANCOS(COMPLEMENTO DE REGISTRO)
                   PadRight(Sacado.Logradouro + ' ' + Sacado.Numero + ' ' +
                            Sacado.Complemento , 40, ' ')                                         + // RUA, NÚMERO E COMPLEMENTO DO SACADO
                   PadRight(Sacado.Bairro, 12, ' ')                                               + // BAIRRO DO SACADO
                   PadLeft(OnlyNumber(Sacado.CEP), 8, '0')                                        + // CEP DO SACADO
                   PadRight(Sacado.Cidade, 15, ' ')                                               + // CIDADE DO SACADO
                   PadRight(Sacado.UF, 2, ' ')                                                    + // UF DO SACADO
                   {Dados do sacador/avalista}
                   PadRight(Sacado.SacadoAvalista.NomeAvalista, 30, ' ')                          + // NOME DO SACADOR/AVALISTA
                   space(4)                                                                       + // COMPLEMENTO DO REGISTRO
                   PadRight(TiraAcentos(LocalPagamento), 55, ' ')                                 + // LOCAL PAGAMENTO
                   PadRight(' ', 55, ' ')                                                         + // LOCAL PAGAMENTO 2
                   '01'                                                                           + // IDENTIF. TIPO DE INSCRIÇÃO DO SACADOR/AVALISTA
                   PadRight(Sacado.SacadoAvalista.CNPJCPF, 15, '0')                               + // NÚMERO DE INSCRIÇÃO DO SACADOR/AVALISTA
                   space(31)                                                                      + // COMPLEMENTO DO REGISTRO
                   IntToStrZero(aRemessa.Count + 1 , 6);                                            // Nº SEQÜENCIAL DO REGISTRO NO ARQUIVO

          aRemessa.Add(UpperCase(wLinha));
          wLinha := MontaInstrucoesCNAB400(ACBrTitulo, aRemessa.Count );

         if not(wLinha = EmptyStr) then
           aRemessa.Add(UpperCase(wLinha));

          //Result := DoMontaInstrucoes2(Result);               // opcional
        end
       else
        {Carteira com registro}
        begin
          wLinha:= '1'                                                                            + // 1 a 1 - IDENTIFICAÇÃO DO REGISTRO TRANSAÇÃO
                   PadLeft(ATipoCedente,2,'0')                                                    + // TIPO DE INSCRIÇÃO DA EMPRESA
                   PadLeft(OnlyNumber(Cedente.CNPJCPF),14,'0')                                    + // Nº DE INSCRIÇÃO DA EMPRESA (CPF/CGC)
                   PadLeft(OnlyNumber(Cedente.Agencia), 4, '0')                                   + // AGÊNCIA MANTENEDORA DA CONTA
                   '00'                                                                           + // COMPLEMENTO DE REGISTRO
                   PadLeft(OnlyNumber(Cedente.Conta), 5, '0')                                     + // NÚMERO DA CONTA CORRENTE DA EMPRESA
                   PadRight(Cedente.ContaDigito, 1)                                               + // DÍGITO DE AUTO CONFERÊNCIA AG/CONTA EMPRESA
                   space(4)                                                                       + // COMPLEMENTO DE REGISTRO
                   '0000'                                                                         + // CÓD.INSTRUÇÃO/ALEGAÇÃO A SER CANCELADA
                   PadRight(SeuNumero, 25, ' ')                                                   + // IDENTIFICAÇÃO DO TÍTULO NA EMPRESA
                   PadLeft(NossoNumero, 8, '0')                                                   + // IDENTIFICAÇÃO DO TÍTULO NO BANCO
                   '0000000000000'                                                                + // QUANTIDADE DE MOEDA VARIÁVEL
                   PadLeft(Carteira,3,' ')                                                        + // NÚMERO DA CARTEIRA NO BANCO
                   space(21)                                                                      + // IDENTIFICAÇÃO DA OPERAÇÃO NO BANCO
                   'I'                                                                            + // CÓDIGO DA CARTEIRA
                   ATipoOcorrencia                                                                + // IDENTIFICAÇÃO DA OCORRÊNCIA
                   PadRight(NumeroDocumento, 10, ' ')                                             + // Nº DO DOCUMENTO DE COBRANÇA (DUPL.,NP ETC.)
                   FormatDateTime('ddmmyy', Vencimento)                                           + // DATA DE VENCIMENTO DO TÍTULO
                   IntToStrZero( round( ValorDocumento * 100), 13)                                + // VALOR NOMINAL DO TÍTULO
                   IntToStrZero(ACBrBanco.Numero, 3)                                              + // Nº DO BANCO NA CÂMARA DE COMPENSAÇÃO
                   '00000'                                                                        + // AGÊNCIA ONDE O TÍTULO SERÁ COBRADO
                   PadLeft(ATipoEspecieDoc, 2, '0')                                               + // ESPÉCIE DO TÍTULO
                   ATipoAceite                                                                    + // IDENTIFICAÇÃO DE TITILO ACEITO OU NÃO ACEITO
                   FormatDateTime('ddmmyy', DataDocumento)                                        + // DATA DA EMISSÃO DO TÍTULO
                   PadLeft(trim(ACBrStr(Instrucao1)), 2, '0')                                     + // 1ª INSTRUÇÃO
                   PadLeft(trim(ACBrStr(Instrucao2)), 2, '0')                                     + // 2ª INSTRUÇÃO
                   IntToStrZero( round(ValorMoraJuros * 100 ), 13)                                + // VALOR DE MORA POR DIA DE ATRASO
                   ADataDesconto                                                                  + // DATA LIMITE PARA CONCESSÃO DE DESCONTO
                   IfThen(ValorDesconto > 0, IntToStrZero( round(ValorDesconto * 100), 13),
                   PadLeft('', 13, '0'))                                                          + // VALOR DO DESCONTO A SER CONCEDIDO
                   IntToStrZero( round(ValorIOF * 100), 13)                                       + // VALOR DO I.O.F. RECOLHIDO P/ NOTAS SEGURO
                   IntToStrZero( round(ValorAbatimento * 100), 13)                                + // VALOR DO ABATIMENTO A SER CONCEDIDO

                   {Dados do sacado}
                   PadLeft(ATipoSacado, 2, '0')                                                   + // IDENTIFICAÇÃO DO TIPO DE INSCRIÇÃO/SACADO
                   PadLeft(OnlyNumber(Sacado.CNPJCPF), 14, '0')                                   + // Nº DE INSCRIÇÃO DO SACADO  (CPF/CGC)
                   PadRight(Sacado.NomeSacado, 30, ' ')                                           + // NOME DO SACADO
                   space(10)                                                                      + // BRANCOS(COMPLEMENTO DE REGISTRO)
                   PadRight(Sacado.Logradouro + ' '+ Sacado.Numero + ' ' +
                            Sacado.Complemento , 40, ' ')                                         + // RUA, NÚMERO E COMPLEMENTO DO SACADO
                   PadRight(Sacado.Bairro, 12, ' ')                                               + // BAIRRO DO SACADO
                   PadLeft(OnlyNumber(Sacado.CEP), 8, '0')                                        + // CEP DO SACADO
                   PadRight(Sacado.Cidade, 15, ' ')                                               + // CIDADE DO SACADO
                   PadRight(Sacado.UF, 2, ' ')                                                    + // UF DO SACADO

                   {Dados do sacador/avalista}
                   PadRight(Sacado.SacadoAvalista.NomeAvalista, 30, ' ')                          + // NOME DO SACADOR/AVALISTA
                   space(4)                                                                       + // COMPLEMENTO DO REGISTRO
                   ADataMoraJuros                                                                 + // DATA DE MORA
                   IfThen((DataProtestoNegativacao <> 0) and (DataProtestoNegativacao > Vencimento),
                        PadLeft(DiasProtestoNegativacao , 2, '0'), '00')+ // PRAZO
                   space(1)                                                                       + // BRANCOS
                   IntToStrZero(aRemessa.Count + 1, 6);                                             // Nº SEQÜENCIAL DO REGISTRO NO ARQUIVO

                   iSequencia := aRemessa.Count + 1;
                   aRemessa.Add(UpperCase(wLinha));
                   //Registro Complemento Detalhe - Multa
                   if PercentualMulta > 0 then
                   begin
                     inc( iSequencia );
                     wLinhaMulta:= '2'                                              + // Tipo de registro - 2 OPCIONAL – COMPLEMENTO DETALHE - MULTA
                                   IfThen(MultaValorFixo,'1','2')                   + // Cocidgo da Multa 1- Cobrar Multa Valor Fixo / 2- Percentual / 0-Não cobrar multa
                                   ifThen((DataMulta > 0),
                                           FormatDateTime('ddmmyyyy',  DataMulta),
                                           Poem_Zeros('',8))                        + // Data da Multa 9(008)
                                   IntToStrZero( round(PercentualMulta * 100 ), 13) + // Valor/Percentual 9(013)
                                   space(371)                                       + // Complemento
                                   IntToStrZero(iSequencia , 6);                      // Sequencial

                     aRemessa.Add(UpperCase(wLinhaMulta));
                   end;

                   //OPCIONAL – COBRANÇA E-MAIL E/OU DADOS DO SACADOR AVALISTA
                   if (Sacado.Email <> '') or (Sacado.SacadoAvalista.CNPJCPF <> '') then
                   begin
                     inc( iSequencia );
                     wLinhaMulta:= '5'                                                          + // 001 - 001 Tipo de registro - 5 IDENTIFICAÇÃO DO REGISTRO TRANSAÇÃO
                                   PadRight(Sacado.Email, 120, ' ')                             + // 002 - 121 ENDEREÇO DE E-MAIL ENDEREÇO DE E-MAIL DO PAGADOR
                                   PadLeft(ATipoSacadoAvalista, 2, '0')                         + // 122 - 123 CÓDIGO DE INSCRIÇÃO IDENT. DO TIPO DE INSCRIÇÃO DO SACADOR/AVALISTA
                                   PadLeft(OnlyNumber(Sacado.SacadoAvalista.CNPJCPF), 14, '0')  + // 124 - 137 NÚMERO DE INSCRIÇÃO NÚMERO DE INSCRIÇÃO DO SACADOR AVALISTA
                                   PadRight(Sacado.SacadoAvalista.Logradouro + ' '              +
                                   Sacado.SacadoAvalista.Numero + ' '                           +
                                   Sacado.SacadoAvalista.Complemento , 40, ' ')                 + // 138 - 177 RUA, Nº E COMPLEMENTO DO SACADOR AVALISTA
                                   PadRight(Sacado.SacadoAvalista.Bairro, 12, ' ')              + // 178 - 189 BAIRRO DO SACADOR AVALISTA
                                   PadLeft(OnlyNumber(Sacado.SacadoAvalista.CEP), 8, '0')       + // 190 - 197 CEP DO SACADOR AVALISTA
                                   PadRight(Sacado.SacadoAvalista.Cidade, 15, ' ')              + // 198 - 212 CIDADE DO SACADOR AVALISTA
                                   PadRight(Sacado.SacadoAvalista.UF, 2, ' ')                   + // 213 - 214 UF (ESTADO) DO SACADOR AVALISTA
                                   space(139)                                                   + // 215 - 353 Brancos
                                   //para se operar com mais de um desconto (depende de cadastramento prévio do indicador 19.0 pelo Itaú, conforme Item 5)
                                   IfThen(ValorDesconto2>0,                                       // Alternativamente este campo poderá ter dois outros usos  (SACADOR/AVALISTA ou 2 e 3 descontos)

                                          FormatDateTime('ddmmyy', DataDesconto2),                // 354 - 359 Data do 2º desconto (DDMMAA)
                                          space(6))                                             +
                                   IfThen(ValorDesconto2>0,
                                          IntToStrZero(round(ValorDesconto2 * 100), 13),          // 360 - 372 Valor do 2º desconto
                                          space(13))                                            +
                                   IfThen(ValorDesconto3>0,
                                          FormatDateTime('ddmmyy', DataDesconto3),                // 373 - 378 Data do 3º desconto (DDMMAA)
                                          space(6))                                             +
                                   IfThen(ValorDesconto3>0,
                                          IntToStrZero(round(ValorDesconto3 * 100), 13),          // 379 - 391 Valor do 3º desconto
                                          space(13))                                            +

                                   space(3)                                                     + // 392 - 394 COMPLEMENTO DE REGISTRO
                                   IntToStrZero(iSequencia , 6);                                  // 395 - 400 Sequencial

                     aRemessa.Add(UpperCase(wLinhaMulta));
                   end;

        end;

        {Registro Híbrido - Bolecode}
        if (NaoEstaVazio(ACBrBoleto.Cedente.PIX.Chave)) then
        begin
          wLinha := '3'                                              + // 001 a 001 - Identificação do registro bolecode (3)
                    PadRight(ACBrBoleto.Cedente.PIX.Chave, 77, ' ')  + // 002 a 078 - Chave Pix (opicional)
                    PadRight(QrCode.txId,  64, ' ')                  + // 079 a 142 - ID DA URL DO QR CODE PIX (opcional)
                    PadRight('', 252, ' ')                           + // 143 a 394 - Brancos
                    IntToStrZero( ARemessa.Count + 1, 6);              // 395 a 400 - Número sequencial do registro
          ARemessa.Text:= ARemessa.Text + UpperCase(wLinha);
        end;


    end;
  end;
end;

procedure TACBrBancoItau.LerRetorno400(ARetorno: TStringList);
var
  ContLinha, CodOCorrencia, I    : Integer;
  MotivoLinha, wMotivoRejeicaoCMD: Integer;
  Linha, rCedente, rDigitoConta  : String ;
  rCNPJCPF,rAgencia,rConta       : String;
  Titulo: TACBrTitulo;
begin

  if StrToIntDef(copy(ARetorno.Strings[0],77,3),-1) <> Numero then
    raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                                   'não é um arquivo de retorno do '+ Nome));

  rCedente     := trim(Copy(ARetorno[0],47,30));
  rAgencia     := trim(Copy(ARetorno[0],27,4));
  rConta       := trim(Copy(ARetorno[0],33,5));
  rDigitoConta := Copy(ARetorno[0],38,1);

  ACBrBanco.ACBrBoleto.NumeroArquivo := StrToIntDef(Copy(ARetorno[0],109,5),0);

  ACBrBanco.ACBrBoleto.DataArquivo   := StringToDateTimeDef(Copy(ARetorno[0],95,2)+'/'+
                                                            Copy(ARetorno[0],97,2)+'/'+
                                                            Copy(ARetorno[0],99,2),0, 'DD/MM/YY' );

  ACBrBanco.ACBrBoleto.DataCreditoLanc := StringToDateTimeDef(Copy(ARetorno[0],114,2)+'/'+
                                                              Copy(ARetorno[0],116,2)+'/'+
                                                              Copy(ARetorno[0],118,2),0, 'DD/MM/YY' );

  case StrToIntDef(Copy(ARetorno[1],2,2),0) of
    1 : rCNPJCPF:= Copy(ARetorno[1],07,11);
    2 : rCNPJCPF:= Copy(ARetorno[1],04,14);
  else
    rCNPJCPF:= Copy(ARetorno[1],4,14);
  end;

  ValidarDadosRetorno(rAgencia, rConta, rCNPJCPF);
  with ACBrBanco.ACBrBoleto do
  begin
    case StrToIntDef(Copy(ARetorno[1],2,2),0) of
      01: Cedente.TipoInscricao:= pFisica;
    else
      Cedente.TipoInscricao:= pJuridica;
    end;

    Cedente.Nome         := rCedente;
    Cedente.CNPJCPF      := rCNPJCPF;
    Cedente.Agencia      := rAgencia;
    Cedente.AgenciaDigito:= '0';
    Cedente.Conta        := rConta;
    Cedente.ContaDigito  := rDigitoConta;

    ACBrBanco.ACBrBoleto.ListadeBoletos.Clear;
  end;

  for ContLinha := 1 to ARetorno.Count - 2 do
  begin
    Linha := ARetorno[ContLinha] ;

    if Linha[1] = '1' then
      Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

    with Titulo do
    begin
      if Linha[1] = '1' then
      begin
        SeuNumero                   := copy(Linha,38,25);
        NumeroDocumento             := copy(Linha,117,10);
        Carteira                    := copy(Linha,83,3);

        OcorrenciaOriginal.Tipo     := CodOcorrenciaToTipo(StrToIntDef(copy(Linha,109,2),0));
        Sacado.NomeSacado           := copy(Linha,325,30);

        if OcorrenciaOriginal.Tipo in [toRetornoInstrucaoProtestoRejeitadaSustadaOuPendente,
                                       toRetornoAlegacaoDoSacado, toRetornoInstrucaoCancelada] then
        begin
          MotivoLinha := 302;
          wMotivoRejeicaoCMD:= StrToIntDef(Copy(Linha, MotivoLinha, 4), 0);

          if wMotivoRejeicaoCMD <> 0 then
          begin
            MotivoRejeicaoComando.Add(Copy(Linha, MotivoLinha, 4));
            CodOcorrencia := StrToIntDef(MotivoRejeicaoComando[0], 0);
            DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo, CodOcorrencia));
          end;
        end
        else
        begin
          MotivoLinha := 378;
        for I := 0 to 3 do
        begin
          wMotivoRejeicaoCMD:= StrToIntDef(Copy(Linha, MotivoLinha, 2),0);

        if wMotivoRejeicaoCMD <> 0 then
        begin
          MotivoRejeicaoComando.Add(Copy(Linha, MotivoLinha, 2));
          CodOcorrencia := StrToIntDef(MotivoRejeicaoComando[I], 0) ;
         DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo, CodOcorrencia));
        end;

          MotivoLinha := MotivoLinha + 2;
        end;
      end;

        DataOcorrencia := StringToDateTimeDef( Copy(Linha,111,2)+'/'+
                                            Copy(Linha,113,2)+'/'+
                                            Copy(Linha,115,2),0, 'DD/MM/YY' );

        {Espécie do documento}
        EspecieDoc := ConverteEspecieDoc(StrToIntDef(Copy(Linha,174,2),0));

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
        NossoNumero          := Copy(Linha,63,8);
        Carteira             := Copy(Linha,83,3);
        ValorDespesaCobranca := StrToFloatDef(Copy(Linha,176,13),0)/100;
        CodigoLiquidacao     := Copy(Linha,393,2);
        CodigoLiquidacaoDescricao := CodigoLiquidacao_Descricao( CodigoLiquidacao );
        // informações do local de pagamento
        Liquidacao.Banco      := StrToIntDef(Copy(Linha,166,3), -1);
        Liquidacao.Agencia    := Copy(Linha,169,4);
        Liquidacao.Origem     := '';

        if StrToIntDef(Copy(Linha,296,6),0) <> 0 then
         DataCredito:= StringToDateTimeDef( Copy(Linha,296,2)+'/'+
                                            Copy(Linha,298,2)+'/'+
                                            Copy(Linha,300,2),0, 'DD/MM/YY' );

        if StrToIntDef(Copy(Linha,111,6),0) <> 0 then
         DataBaixa := StringToDateTimeDef(Copy(Linha,111,2)+'/'+
                                          Copy(Linha,113,2)+'/'+
                                          Copy(Linha,115,2),0,'DD/MM/YY');

      end;
      if Linha[1] = '3' then
        QrCode.emv := trim(copy(Linha,2,390));
    end;
   end;
end;

function TACBrBancoItau.TipoOcorrenciaToDescricao(
  const TipoOcorrencia: TACBrTipoOcorrencia): String;
var
 CodOcorrencia: Integer;
begin
  Result := '';
  CodOcorrencia := StrToIntDef(TipoOCorrenciaToCod(TipoOcorrencia),0);

  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
  begin
    case CodOcorrencia of
      94: Result := '94-Confirma Recebimento de Instrução de não Negativar';
    end;
  end
  else
  begin
    case CodOcorrencia of
      07: Result := '07-Liquidação Parcial – Cobrança Inteligente';
      59: Result := '59-Baixa Por Crédito em C/C Através do Sispag';
      64: Result := '64-Entrada Confirmada com Rateio de Crédito';
      65: Result := '65-Pagamento com Cheque – Aguardando Compensação';
      69: Result := '69-Cheque Devolvido';
      71: Result := '71-Entrada Registrada Aguardando Avaliação';
      72: Result := '72-Baixa Por Crédito em C/C Através do Sispag Sem Título Correspondente';
      73: Result := '73-Confirmação de Entrada na Cobrança Simples – Entrada não Aceita na Cobrança Contratual';
      76: Result := '76-Cheque Compensado';
    end;
  end;

  if (Result <> '') then
  begin
    Result := ACBrSTr(Result);
    Exit;
  end;

  case CodOcorrencia of
    02: Result := '02-Entrada Confirmada';
    03: Result := '03-Entrada Rejeitada';
    04: Result := '04-Alteração de Dados - Nova Entrada ou Alteração/Exclusão de Dados Acatada';
    05: Result := '05-Alteração de Dados - Baixa';
    06: Result := '06-Liquidação Normal';
    07: Result := '07-Liquidação Parcial - Cobrança Inteligente' ;
    08: Result := '08-Liquidação em Cartório';
    09: Result := '09-Baixa Simples' ;
    10: Result := '10-Baixa por ter sido Liquidado' ;
    11: Result := '11-Em Ser';
    12: Result := '12-Abatimento Concedido';
    13: Result := '13-Abatimento Cancelado';
    14: Result := '14-Vencimento Alterado';
    15: Result := '15-Baixas Rejeitadas';
    16: Result := '16-Instruções Rejeitadas';
    17: Result := '17-Alteração/Exclusão de Dados Rejeitados';
    18: Result := '18-Cobrança Contratual - Instruções/Alterações Rejeitadas/Pendentes';
    19: Result := '19-Confirma Recebimento de Instrução de Protesto';
    20: Result := '20-Confirma Recebimento de Instrução de Sustação de Protesto/Tarifa';
    21: Result := '21-Confirma Recebimento de Instrução de não Protestar';
    23: Result := '23-Título Enviado A Cartório/Tarifa';
    24: Result := '24-Instrução de Protesto Rejeitada / Sustada / Pendente';
    25: Result := '25-Alegações do Pagador';
    26: Result := '26-Tarifa de Aviso de Cobrança';
    27: Result := '27-Tarifa de Extrato Posição' ;
    28: Result := '28-Tarifa de Relação das Liquidações';
    29: Result := '29-Tarifa de Manutenção de Títulos Vencidos';
    30: Result := '30-Débito Mensal de Tarifas';
    32: Result := '32-Baixa por ter sido Protestado';
    33: Result := '33-Custas de Protesto';
    34: Result := '34-Custas de Sustação';
    35: Result := '35-Custas de Cartório Distribuidor';
    36: Result := '36-Custas de Edital';
    37: Result := '37-Tarifa de Emissão de Boleto/Tarifa de Envio de Duplicata';
    38: Result := '38-Tarifa de Instrução';
    39: Result := '39-Tarifa de Ocorrências';
    40: Result := '40-Tarifa Mensal de Emissão de Boleto/Tarifa Mensal de Envio De Duplicata';
    41: Result := '41-Débito Mensal de Tarifas - Extrato de Posição';
    42: Result := '42-Débito Mensal de Tarifas - Outras Instruções';
    43: Result := '43-Débito Mensal de Tarifas - Manutenção de Títulos Vencidos';
    44: Result := '44-Débito Mensal de Tarifas - Outras Ocorrências';
    45: Result := '45-Débito Mensal de Tarifas - Protesto';
    46: Result := '46-Débito Mensal de Tarifas - Sustação de Protesto';
    47: Result := '47-Baixa com Transferência para Desconto';
    48: Result := '48-Custas de Sustação Judicial';
    49: Result := '49-Alteração de dados extras';
    51: Result := '51-Tarifa Mensal Referente a Entradas Bancos Correspondentes na Carteira';
    52: Result := '52-Tarifa Mensal Baixas na Carteira';
    53: Result := '53-Tarifa Mensal Baixas em Bancos Correspondentes na Carteira';
    54: Result := '54-Tarifa Mensal de Liquidações na Carteira';
    55: Result := '55-Tarifa Mensal de Liquidações em Bancos Correspondentes na Carteira';
    56: Result := '56-Custas de Irregularidade';
    57: Result := '57-Instrução Cancelada';
    60: Result := '60-Entrada Rejeitada Carnê';
    61: Result := '61-Tarifa Emissão Aviso de Movimentação de Títulos';
    62: Result := '62-Débito Mensal de Tarifa - Aviso de Movimentação de Títulos';
    63: Result := '63-Título Sustado Judicialmente';
    74: Result := '74-Instrução de Negativação Expressa Rejeitada';
    75: Result := '75-Confirmação de Recebimento de Instrução de Entrada em Negativação Expressa';
    77: Result := '77-Confirmação de Recebimento de Instrução de Exclusão de Entrada em Negativação Expressa';
    78: Result := '78-Confirmação de Recebimento de Instrução de Cancelamento de Negativação Expressa';
    79: Result := '79-Negativação Expressa Informacional';
    80: Result := '80-Confirmação de Entrada em Negativação Expressa – Tarifa';
    82: Result := '82-Confirmação do Cancelamento de Negativação Expressa – Tarifa';
    83: Result := '83-Confirmação de Exclusão de Entrada em Negativação Expressa Por Liquidação – Tarifa';
    85: Result := '85-Tarifa Por Boleto (Até 03 Envios) Cobrança Ativa Eletrônica';
    86: Result := '86-Tarifa Email Cobrança Ativa Eletrônica';
    87: Result := '87-Tarifa SMS Cobrança Ativa Eletrônica';
    88: Result := '88-Tarifa Mensal Por Boleto (Até 03 Envios) Cobrança Ativa Eletrônica';
    89: Result := '89-Tarifa Mensal Email Cobrança Ativa Eletrônica';
    90: Result := '90-Tarifa Mensal SMS Cobrança Ativa Eletrônica';
    91: Result := '91-Tarifa Mensal de Exclusão de Entrada de Negativação Expressa';
    92: Result := '92-Tarifa Mensal de Cancelamento de Negativação Expressa';
    93: Result := '93-Tarifa Mensal de Exclusão de Negativação Expressa Por Liquidação';
  end;

  Result := ACBrSTr(Result);
end;

function TACBrBancoItau.CodOcorrenciaToTipo(
  const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  Result := toTipoOcorrenciaNenhum;

  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
  begin
    case CodOcorrencia of
      94: Result := toRetornoConfirmaRecebimentoInstrucaoNaoNegativar;
    end;
  end
  else
  begin
    case CodOcorrencia of
      07: Result := toRetornoLiquidadoParcialmente;
      59: Result := toRetornoBaixaCreditoCCAtravesSispag;
      64: Result := toRetornoEntradaConfirmadaRateioCredito;
      65: Result := toRetornoChequePendenteCompensacao;
      69: Result := toRetornoChequeDevolvido;
      71: Result := toRetornoEntradaRegistradaAguardandoAvaliacao;
      72: Result := toRetornoBaixaCreditoCCAtravesSispagSemTituloCorresp;
      73: Result := toRetornoConfirmacaoEntradaCobrancaSimples;
      76: Result := toRetornoChequeCompensado;
    end;
  end;

  if (Result <> toTipoOcorrenciaNenhum) then
    Exit;

  case CodOcorrencia of
    02: Result := toRetornoRegistroConfirmado;
    03: Result := toRetornoRegistroRecusado;
    04: Result := toRetornoAlteracaoDadosNovaEntrada;
    05: Result := toRetornoAlteracaoDadosBaixa;
    06: Result := toRetornoLiquidado;
    08: Result := toRetornoLiquidadoEmCartorio;
    09: Result := toRetornoBaixaSimples;
    10: Result := toRetornoBaixaPorTerSidoLiquidado;
    11: Result := toRetornoTituloEmSer;
    12: Result := toRetornoAbatimentoConcedido;
    13: Result := toRetornoAbatimentoCancelado;
    14: Result := toRetornoVencimentoAlterado;
    15: Result := toRetornoBaixaRejeitada;
    16: Result := toRetornoInstrucaoRejeitada;
    17: Result := toRetornoAlteracaoDadosRejeitados;
    18: Result := toRetornoCobrancaContratual;
    19: Result := toRetornoRecebimentoInstrucaoProtestar;
    20: Result := toRetornoRecebimentoInstrucaoSustarProtesto;
    21: Result := toRetornoRecebimentoInstrucaoNaoProtestar;
    23: Result := toRetornoEncaminhadoACartorio;
    24: Result := toRetornoInstrucaoProtestoRejeitadaSustadaOuPendente;
    25: Result := toRetornoAlegacaoDoSacado;
    26: Result := toRetornoTarifaAvisoCobranca;
    27: Result := toRetornoTarifaExtratoPosicao;
    28: Result := toRetornoTarifaDeRelacaoDasLiquidacoes;
    29: Result := toRetornoTarifaDeManutencaoDeTitulosVencidos;
    30: Result := toRetornoDebitoTarifas;
    32: Result := toRetornoBaixaPorProtesto;
    33: Result := toRetornoCustasProtesto;
    34: Result := toRetornoCustasSustacao;
    35: Result := toRetornoCustasCartorioDistribuidor;
    36: Result := toRetornoCustasEdital;
    37: Result := toRetornoTarifaEmissaoBoletoEnvioDuplicata;
    38: Result := toRetornoTarifaInstrucao;
    39: Result := toRetornoTarifaOcorrencias;
    40: Result := toRetornoTarifaMensalEmissaoBoletoEnvioDuplicata;
    41: Result := toRetornoDebitoMensalTarifasExtradoPosicao;
    42: Result := toRetornoDebitoMensalTarifasOutrasInstrucoes;
    43: Result := toRetornoDebitoMensalTarifasManutencaoTitulosVencidos;
    44: Result := toRetornoDebitoMensalTarifasOutrasOcorrencias;
    45: Result := toRetornoDebitoMensalTarifasProtestos;
    46: Result := toRetornoDebitoMensalTarifasSustacaoProtestos;
    47: Result := toRetornoBaixaTransferenciaParaDesconto;
    48: Result := toRetornoCustasSustacaoJudicial;
    51: Result := toRetornoTarifaMensalRefEntradasBancosCorrespCarteira;
    52: Result := toRetornoTarifaMensalBaixasCarteira;
    53: Result := toRetornoTarifaMensalBaixasBancosCorrespCarteira;
    54: Result := toRetornoTarifaMensalLiquidacoesCarteira;
    55: Result := toRetornoTarifaMensalLiquidacoesBancosCorrespCarteira;
    56: Result := toRetornoCustasIrregularidade;
    57: Result := toRetornoInstrucaoCancelada;
    60: Result := toRetornoEntradaRejeitadaCarne;
    61: Result := toRetornoTarifaEmissaoAvisoMovimentacaoTitulos;
    62: Result := toRetornoDebitoMensalTarifaAvisoMovimentacaoTitulos;
    63: Result := toRetornoTituloSustadoJudicialmente;
    74: Result := toRetornoInstrucaoNegativacaoExpressaRejeitada;
    75: Result := toRetornoConfRecebimentoInstEntradaNegativacaoExpressa;
    77: Result := toRetornoConfRecebimentoInstExclusaoEntradaNegativacaoExpressa;
    78: Result := toRetornoConfRecebimentoInstCancelamentoNegativacaoExpressa;
    79: Result := toRetornoNegativacaoExpressaInformacional;
    80: Result := toRetornoConfEntradaNegativacaoExpressaTarifa;
    82: Result := toRetornoConfCancelamentoNegativacaoExpressaTarifa;
    83: Result := toRetornoConfExclusaoEntradaNegativacaoExpressaPorLiquidacaoTarifa;
    85: Result := toRetornoTarifaPorBoletoAte03EnvioCobrancaAtivaEletronica;
    86: Result := toRetornoTarifaEmailCobrancaAtivaEletronica;
    87: Result := toRetornoTarifaSMSCobrancaAtivaEletronica;
    88: Result := toRetornoTarifaMensalPorBoletoAte03EnvioCobrancaAtivaEletronica;
    89: Result := toRetornoTarifaMensalEmailCobrancaAtivaEletronica;
    90: Result := toRetornoTarifaMensalSMSCobrancaAtivaEletronica;
    91: Result := toRetornoTarifaMensalExclusaoEntradaNegativacaoExpressa;
    92: Result := toRetornoTarifaMensalCancelamentoNegativacaoExpressa;
    93: Result := toRetornoTarifaMensalExclusaoNegativacaoExpressaPorLiquidacao;
  else
    Result := toRetornoOutrasOcorrencias;
  end;
end;

function TACBrBancoItau.TipoOCorrenciaToCod(
  const TipoOcorrencia: TACBrTipoOcorrencia): String;
begin
  Result := '';

  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
  begin
    case TipoOcorrencia of
      toRetornoConfirmaRecebimentoInstrucaoNaoNegativar                : Result := '94';
    end;
  end
  else
  begin
    case TipoOcorrencia of
      toRetornoLiquidadoParcialmente                                   : Result := '07';
      toRetornoBaixaCreditoCCAtravesSispag                             : Result := '59';
      toRetornoEntradaConfirmadaRateioCredito                          : Result := '64';
      toRetornoChequePendenteCompensacao                               : Result := '65';
      toRetornoChequeDevolvido                                         : Result := '69';
      toRetornoEntradaRegistradaAguardandoAvaliacao                    : Result := '71';
      toRetornoBaixaCreditoCCAtravesSispagSemTituloCorresp             : Result := '72';
      toRetornoConfirmacaoEntradaCobrancaSimples                       : Result := '73';
      toRetornoChequeCompensado                                        : Result := '76';
    end;
  end;

  if (Result <> '') then
    Exit;

  case TipoOcorrencia of
    toRetornoRegistroConfirmado                                        : Result := '02';
    toRetornoRegistroRecusado                                          : Result := '03';
    toRetornoAlteracaoDadosNovaEntrada                                 : Result := '04';
    toRetornoAlteracaoDadosBaixa                                       : Result := '05';
    toRetornoLiquidado                                                 : Result := '06';
    toRetornoLiquidadoEmCartorio                                       : Result := '08';
    toRetornoBaixaSimples                                              : Result := '09';
    toRetornoBaixaPorTerSidoLiquidado                                  : Result := '10';
    toRetornoTituloEmSer                                               : Result := '11';
    toRetornoAbatimentoConcedido                                       : Result := '12';
    toRetornoAbatimentoCancelado                                       : Result := '13';
    toRetornoVencimentoAlterado                                        : Result := '14';
    toRetornoBaixaRejeitada                                            : Result := '15';
    toRetornoInstrucaoRejeitada                                        : Result := '16';
    toRetornoAlteracaoDadosRejeitados                                  : Result := '17';
    toRetornoCobrancaContratual                                        : Result := '18';
    toRetornoRecebimentoInstrucaoProtestar                             : Result := '19';
    toRetornoRecebimentoInstrucaoSustarProtesto                        : Result := '20';
    toRetornoRecebimentoInstrucaoNaoProtestar                          : Result := '21';
    toRetornoEncaminhadoACartorio                                      : Result := '23';
    toRetornoInstrucaoProtestoRejeitadaSustadaOuPendente               : Result := '24';
    toRetornoAlegacaoDoSacado                                          : Result := '25';
    toRetornoTarifaAvisoCobranca                                       : Result := '26';
    toRetornoTarifaExtratoPosicao                                      : Result := '27';
    toRetornoTarifaDeRelacaoDasLiquidacoes                             : Result := '28';
    toRetornoTarifaDeManutencaoDeTitulosVencidos                       : Result := '29';
    toRetornoDebitoTarifas                                             : Result := '30';
    toRetornoBaixaPorProtesto                                          : Result := '32';
    toRetornoCustasProtesto                                            : Result := '33';
    toRetornoCustasSustacao                                            : Result := '34';
    toRetornoCustasCartorioDistribuidor                                : Result := '35';
    toRetornoCustasEdital                                              : Result := '36';
    toRetornoTarifaEmissaoBoletoEnvioDuplicata                         : Result := '37';
    toRetornoTarifaInstrucao                                           : Result := '38';
    toRetornoTarifaOcorrencias                                         : Result := '39';
    toRetornoTarifaMensalEmissaoBoletoEnvioDuplicata                   : Result := '40';
    toRetornoDebitoMensalTarifasExtradoPosicao                         : Result := '41';
    toRetornoDebitoMensalTarifasOutrasInstrucoes                       : Result := '42';
    toRetornoDebitoMensalTarifasManutencaoTitulosVencidos              : Result := '43';
    toRetornoDebitoMensalTarifasOutrasOcorrencias                      : Result := '44';
    toRetornoDebitoMensalTarifasProtestos                              : Result := '45';
    toRetornoDebitoMensalTarifasSustacaoProtestos                      : Result := '46';
    toRetornoBaixaTransferenciaParaDesconto                            : Result := '47';
    toRetornoCustasSustacaoJudicial                                    : Result := '48';
    toRetornoTarifaMensalRefEntradasBancosCorrespCarteira              : Result := '51';
    toRetornoTarifaMensalBaixasCarteira                                : Result := '52';
    toRetornoTarifaMensalBaixasBancosCorrespCarteira                   : Result := '53';
    toRetornoTarifaMensalLiquidacoesCarteira                           : Result := '54';
    toRetornoTarifaMensalLiquidacoesBancosCorrespCarteira              : Result := '55';
    toRetornoCustasIrregularidade                                      : Result := '56';
    toRetornoInstrucaoCancelada                                        : Result := '57';
    toRetornoEntradaRejeitadaCarne                                     : Result := '60';
    toRetornoTarifaEmissaoAvisoMovimentacaoTitulos                     : Result := '61';
    toRetornoDebitoMensalTarifaAvisoMovimentacaoTitulos                : Result := '62';
    toRetornoTituloSustadoJudicialmente                                : Result := '63';
    toRetornoInstrucaoNegativacaoExpressaRejeitada                     : Result := '74';
    toRetornoConfRecebimentoInstEntradaNegativacaoExpressa             : Result := '75';
    toRetornoConfRecebimentoInstExclusaoEntradaNegativacaoExpressa     : Result := '77';
    toRetornoConfRecebimentoInstCancelamentoNegativacaoExpressa        : Result := '78';
    toRetornoNegativacaoExpressaInformacional                          : Result := '79';
    toRetornoConfEntradaNegativacaoExpressaTarifa                      : Result := '80';
    toRetornoConfCancelamentoNegativacaoExpressaTarifa                 : Result := '82';
    toRetornoConfExclusaoEntradaNegativacaoExpressaPorLiquidacaoTarifa : Result := '83';
    toRetornoTarifaPorBoletoAte03EnvioCobrancaAtivaEletronica          : Result := '85';
    toRetornoTarifaEmailCobrancaAtivaEletronica                        : Result := '86';
    toRetornoTarifaSMSCobrancaAtivaEletronica                          : Result := '87';
    toRetornoTarifaMensalPorBoletoAte03EnvioCobrancaAtivaEletronica    : Result := '88';
    toRetornoTarifaMensalEmailCobrancaAtivaEletronica                  : Result := '89';
    toRetornoTarifaMensalSMSCobrancaAtivaEletronica                    : Result := '90';
    toRetornoTarifaMensalExclusaoEntradaNegativacaoExpressa            : Result := '91';
    toRetornoTarifaMensalCancelamentoNegativacaoExpressa               : Result := '92';
    toRetornoTarifaMensalExclusaoNegativacaoExpressaPorLiquidacao      : Result := '93';
  else
    Result := '02';
  end;
end;

function TACBrBancoItau.CodMotivoRejeicaoToDescricao(
  const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: Integer): String;
begin
  case TipoOcorrencia of
  
      //Tabela 1
      toRetornoRegistroRecusado, toRetornoEntradaRejeitadaCarne:
      case CodMotivo  of
         03: Result := 'AG. COBRADORA -NÃO FOI POSSÍVEL ATRIBUIR A AGÊNCIA PELO CEP OU CEP INVÁLIDO';
         04: Result := 'ESTADO -SIGLA DO ESTADO INVÁLIDA';
         05: Result := 'DATA VENCIMENTO -PRAZO DA OPERAÇÃO MENOR QUE PRAZO MÍNIMO OU MAIOR QUE O MÁXIMO';
         07: Result := 'VALOR DO TÍTULO -VALOR DO TÍTULO MAIOR QUE 10.000.000,00';
         08: Result := 'NOME DO SACADO -NÃO INFORMADO OU DESLOCADO';
         09: Result := 'AGENCIA/CONTA -AGÊNCIA ENCERRADA';
         10: Result := 'LOGRADOURO -NÃO INFORMADO OU DESLOCADO';
         11: Result := 'CEP -CEP NÃO NUMÉRICO';
         12: Result := 'SACADOR / AVALISTA -NOME NÃO INFORMADO OU DESLOCADO (BANCOS CORRESPONDENTES)';
         13: Result := 'ESTADO/CEP -CEP INCOMPATÍVEL COM A SIGLA DO ESTADO';
         14: Result := 'NOSSO NÚMERO -NOSSO NÚMERO JÁ REGISTRADO NO CADASTRO DO BANCO OU FORA DA FAIXA';
         15: Result := 'NOSSO NÚMERO -NOSSO NÚMERO EM DUPLICIDADE NO MESMO MOVIMENTO';
         18: Result := 'DATA DE ENTRADA -DATA DE ENTRADA INVÁLIDA PARA OPERAR COM ESTA CARTEIRA';
         19: Result := 'OCORRÊNCIA -OCORRÊNCIA INVÁLIDA';
         21: Result := 'AG. COBRADORA - CARTEIRA NÃO ACEITA DEPOSITÁRIA CORRESPONDENTE/'+
                       'ESTADO DA AGÊNCIA DIFERENTE DO ESTADO DO SACADO/'+
                       'AG. COBRADORA NÃO CONSTA NO CADASTRO OU ENCERRANDO';
         22: Result := 'CARTEIRA -CARTEIRA NÃO PERMITIDA (NECESSÁRIO CADASTRAR FAIXA LIVRE)';
         26: Result := 'AGÊNCIA/CONTA -AGÊNCIA/CONTA NÃO LIBERADA PARA OPERAR COM COBRANÇA';
         27: Result := 'CNPJ INAPTO -CNPJ DO CEDENTE INAPTO';
         29: Result := 'CÓDIGO EMPRESA -CATEGORIA DA CONTA INVÁLIDA';
         30: Result := 'ENTRADA BLOQUEADA -ENTRADAS BLOQUEADAS, CONTA SUSPENSA EM COBRANÇA';
         31: Result := 'AGÊNCIA/CONTA -CONTA NÃO TEM PERMISSÃO PARA PROTESTAR (CONTATE SEU GERENTE)';
         35: Result := 'VALOR DO IOF -IOF MAIOR QUE 5%';
         36: Result := 'QTDADE DE MOEDA -QUANTIDADE DE MOEDA INCOMPATÍVEL COM VALOR DO TÍTULO';
         37: Result := 'CNPJ/CPF DO SACADO -NÃO NUMÉRICO OU IGUAL A ZEROS';
         42: Result := 'NOSSO NÚMERO -NOSSO NÚMERO FORA DE FAIXA';
         52: Result := 'AG. COBRADORA -EMPRESA NÃO ACEITA BANCO CORRESPONDENTE';
         53: Result := 'AG. COBRADORA -EMPRESA NÃO ACEITA BANCO CORRESPONDENTE - COBRANÇA MENSAGEM';
         54: Result := 'DATA DE VENCTO -BANCO CORRESPONDENTE - TÍTULO COM VENCIMENTO INFERIOR A 15 DIAS';
         55: Result := 'DEP/BCO CORRESP -CEP NÃO PERTENCE À DEPOSITÁRIA INFORMADA';
         56: Result := 'DT VENCTO/BCO CORRESP -VENCTO SUPERIOR A 180 DIAS DA DATA DE ENTRADA';
         57: Result := 'DATA DE VENCTO -CEP SÓ DEPOSITÁRIA BCO DO BRASIL COM VENCTO INFERIOR A 8 DIAS';
         60: Result := 'ABATIMENTO -VALOR DO ABATIMENTO INVÁLIDO';
         61: Result := 'JUROS DE MORA -JUROS DE MORA MAIOR QUE O PERMITIDO';
         62: Result := 'DESCONTO -VALOR DO DESCONTO MAIOR QUE VALOR DO TÍTULO';
         63: Result := 'DESCONTO DE ANTECIPAÇÃO -VALOR DA IMPORTÂNCIA POR DIA DE DESCONTO (IDD) NÃO PERMITIDO';
         64: Result := 'DATA DE EMISSÃO -DATA DE EMISSÃO DO TÍTULO INVÁLIDA';
         65: Result := 'TAXA FINANCTO -TAXA INVÁLIDA (VENDOR)';
         66: Result := 'DATA DE VENCTO -INVALIDA/FORA DE PRAZO DE OPERAÇÃO (MÍNIMO OU MÁXIMO)';
         67: Result := 'VALOR/QTIDADE -VALOR DO TÍTULO/QUANTIDADE DE MOEDA INVÁLIDO';
         68: Result := 'CARTEIRA -CARTEIRA INVÁLIDA';
         69: Result := 'CARTEIRA -CARTEIRA INVÁLIDA PARA TÍTULOS COM RATEIO DE CRÉDITO';
         70: Result := 'AGÊNCIA/CONTA -CEDENTE NÃO CADASTRADO PARA FAZER RATEIO DE CRÉDITO';
         78: Result := 'AGÊNCIA/CONTA -DUPLICIDADE DE AGÊNCIA/CONTA BENEFICIÁRIA DO RATEIO DE CRÉDITO';
         80: Result := 'AGÊNCIA/CONTA -QUANTIDADE DE CONTAS BENEFICIÁRIAS DO RATEIO MAIOR DO QUE O PERMITIDO (MÁXIMO DE 30 CONTAS POR TÍTULO)';
         81: Result := 'AGÊNCIA/CONTA -CONTA PARA RATEIO DE CRÉDITO INVÁLIDA / NÃO PERTENCE AO ITAÚ';
         82: Result := 'DESCONTO/ABATI-MENTO -DESCONTO/ABATIMENTO NÃO PERMITIDO PARA TÍTULOS COM RATEIO DE CRÉDITO';
         83: Result := 'VALOR DO TÍTULO -VALOR DO TÍTULO MENOR QUE A SOMA DOS VALORES ESTIPULADOS PARA RATEIO';
         84: Result := 'AGÊNCIA/CONTA -AGÊNCIA/CONTA BENEFICIÁRIA DO RATEIO É A CENTRALIZADORA DE CRÉDITO DO CEDENTE';
         85: Result := 'AGÊNCIA/CONTA -AGÊNCIA/CONTA DO CEDENTE É CONTRATUAL / RATEIO DE CRÉDITO NÃO PERMITIDO';
         86: Result := 'TIPO DE VALOR -CÓDIGO DO TIPO DE VALOR INVÁLIDO / NÃO PREVISTO PARA TÍTULOS COM RATEIO DE CRÉDITO';
         87: Result := 'AGÊNCIA/CONTA -REGISTRO TIPO 4 SEM INFORMAÇÃO DE AGÊNCIAS/CONTAS BENEFICIÁRIAS DO RATEIO';
         90: Result := 'NRO DA LINHA -COBRANÇA MENSAGEM - NÚMERO DA LINHA DA MENSAGEM INVÁLIDO';
         97: Result := 'SEM MENSAGEM -COBRANÇA MENSAGEM SEM MENSAGEM (SÓ DE CAMPOS FIXOS), PORÉM COM REGISTRO DO TIPO 7 OU 8';
         98: Result := 'FLASH INVÁLIDO -REGISTRO MENSAGEM SEM FLASH CADASTRADO OU FLASH INFORMADO DIFERENTE DO CADASTRADO';
         99: Result := 'FLASH INVÁLIDO -CONTA DE COBRANÇA COM FLASH CADASTRADO E SEM REGISTRO DE MENSAGEM CORRESPONDENTE';
         91: Result := 'DAC -DAC AGÊNCIA / CONTA CORRENTE INVÁLIDO';
         92: Result := 'DAC -DAC AGÊNCIA/CONTA/CARTEIRA/NOSSO NÚMERO INVÁLIDO';
         93: Result := 'ESTADO -SIGLA ESTADO INVÁLIDA';
         94: Result := 'ESTADO -SIGLA ESTADA INCOMPATÍVEL COM CEP DO SACADO';
         95: Result := 'CEP -CEP DO SACADO NÃO NUMÉRICO OU INVÁLIDO';
         96: Result := 'ENDEREÇO -ENDEREÇO / NOME / CIDADE SACADO INVÁLIDO';
      else
         Result := IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

      //Tabela 2
      toRetornoAlteracaoDadosRejeitados:
      case CodMotivo of
         02: Result := 'AGÊNCIA COBRADORA INVÁLIDA OU COM O MESMO CONTEÚDO';
         04: Result := 'SIGLA DO ESTADO INVÁLIDA';
         05: Result := 'DATA DE VENCIMENTO INVÁLIDA OU COM O MESMO CONTEÚDO';
         06: Result := 'VALOR DO TÍTULO COM OUTRA ALTERAÇÃO SIMULTÂNEA';
         08: Result := 'NOME DO SACADO COM O MESMO CONTEÚDO';
         09: Result := 'AGÊNCIA/CONTA INCORRETA';
         11: Result := 'CEP INVÁLIDO';
         12: Result := 'NÚMERO INSCRIÇÃO INVÁLIDO DO SACADOR AVALISTA';
         13: Result := 'SEU NÚMERO COM O MESMO CONTEÚDO';
         16: Result := 'ABATIMENTO/ALTERAÇÃO DO VALOR DO TÍTULO OU SOLICITAÇÃO DE BAIXA BLOQUEADA';
         20: Result := 'ESPÉCIE INVÁLIDA';
         21: Result := 'AGÊNCIA COBRADORA NÃO CONSTA NO CADASTRO DE DEPOSITÁRIA OU EM ENCERRAMENTO';
         23: Result := 'DATA DE EMISSÃO DO TÍTULO INVÁLIDA OU COM MESMO CONTEÚDO';
         41: Result := 'CAMPO ACEITE INVÁLIDO OU COM MESMO CONTEÚDO';
         42: Result := 'ALTERAÇÃO INVÁLIDA PARA TÍTULO VENCIDO';
         43: Result := 'ALTERAÇÃO BLOQUEADA – VENCIMENTO JÁ ALTERADO';
         53: Result := 'INSTRUÇÃO COM O MESMO CONTEÚDO';
         54: Result := 'DATA VENCIMENTO PARA BANCOS CORRESPONDENTES INFERIOR AO ACEITO PELO BANCO';
         55: Result := 'ALTERAÇÕES IGUAIS PARA O MESMO CONTROLE (AGÊNCIA/CONTA/CARTEIRA/NOSSO NÚMERO)';
         56: Result := 'CGC/CPF INVÁLIDO NÃO NUMÉRICO OU ZERADO';
         57: Result := 'PRAZO DE VENCIMENTO INFERIOR A 15 DIAS';
         60: Result := 'VALOR DE IOF - ALTERAÇÃO NÃO PERMITIDA PARA CARTEIRAS DE N.S. - MOEDA VARIÁVEL';
         61: Result := 'TÍTULO JÁ BAIXADO OU LIQUIDADO OU NÃO EXISTE TÍTULO CORRESPONDENTE NO SISTEMA';
         66: Result := 'ALTERAÇÃO NÃO PERMITIDA PARA CARTEIRAS DE NOTAS DE SEGUROS - MOEDA VARIÁVEL';
         67: Result := 'NOME INVÁLIDO DO SACADOR AVALISTA';
         72: Result := 'ENDEREÇO INVÁLIDO – SACADOR AVALISTA';
         73: Result := 'BAIRRO INVÁLIDO – SACADOR AVALISTA';
         74: Result := 'CIDADE INVÁLIDA – SACADOR AVALISTA';
         75: Result := 'SIGLA ESTADO INVÁLIDO – SACADOR AVALISTA';
         76: Result := 'CEP INVÁLIDO – SACADOR AVALISTA';
         81: Result := 'ALTERAÇÃO BLOQUEADA - TÍTULO COM PROTESTO';
         87: Result := 'ALTERAÇÃO BLOQUEADA – TÍTULO COM RATEIO DE CRÉDITO';
      else
         Result := IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

      //Tabela 3
      toRetornoInstrucaoRejeitada:
      case CodMotivo of
         01: Result := 'INSTRUÇÃO/OCORRÊNCIA NÃO EXISTENTE';
         03: Result := 'CONTA NÃO TEM PERMISSÃO PARA PROTESTAR (CONTATE SEU GERENTE)';
         06: Result := 'NOSSO NÚMERO IGUAL A ZEROS';
         09: Result := 'CGC/CPF DO SACADOR/AVALISTA INVÁLIDO';
         10: Result := 'VALOR DO ABATIMENTO IGUAL OU MAIOR QUE O VALOR DO TÍTULO';
         11: Result := 'SEGUNDA INSTRUÇÃO/OCORRÊNCIA NÃO EXISTENTE';
         14: Result := 'REGISTRO EM DUPLICIDADE';
         15: Result := 'CNPJ/CPF INFORMADO SEM NOME DO SACADOR/AVALISTA';
         19: Result := 'VALOR DO ABATIMENTO MAIOR QUE 90% DO VALOR DO TÍTULO';
         20: Result := 'EXISTE SUSTACAO DE PROTESTO PENDENTE PARA O TITULO';
         21: Result := 'TÍTULO NÃO REGISTRADO NO SISTEMA';
         22: Result := 'TÍTULO BAIXADO OU LIQUIDADO';
         23: Result := 'INSTRUÇÃO NÃO ACEITA POR TER SIDO EMITIDO ÚLTIMO AVISO AO SACADO';
         24: Result := 'INSTRUÇÃO INCOMPATÍVEL - EXISTE INSTRUÇÃO DE PROTESTO PARA O TÍTULO';
         25: Result := 'INSTRUÇÃO INCOMPATÍVEL - NÃO EXISTE INSTRUÇÃO DE PROTESTO PARA O TÍTULO';
         26: Result := 'INSTRUÇÃO NÃO ACEITA POR TER SIDO EMITIDO ÚLTIMO AVISO AO SACADO';
         27: Result := 'INSTRUÇÃO NÃO ACEITA POR NÃO TER SIDO EMITIDA A ORDEM DE PROTESTO AO CARTÓRIO';
         28: Result := 'JÁ EXISTE UMA MESMA INSTRUÇÃO CADASTRADA ANTERIORMENTE PARA O TÍTULO';
         29: Result := 'VALOR LÍQUIDO + VALOR DO ABATIMENTO DIFERENTE DO VALOR DO TÍTULO REGISTRADO, OU VALOR'+
                       'DO ABATIMENTO MAIOR QUE 90% DO VALOR DO TÍTULO';
         30: Result := 'EXISTE UMA INSTRUÇÃO DE NÃO PROTESTAR ATIVA PARA O TÍTULO';
         31: Result := 'EXISTE UMA OCORRÊNCIA DO SACADO QUE BLOQUEIA A INSTRUÇÃO';
         32: Result := 'DEPOSITÁRIA DO TÍTULO = 9999 OU CARTEIRA NÃO ACEITA PROTESTO';
         33: Result := 'ALTERAÇÃO DE VENCIMENTO IGUAL À REGISTRADA NO SISTEMA OU QUE TORNA O TÍTULO VENCIDO';
         34: Result := 'INSTRUÇÃO DE EMISSÃO DE AVISO DE COBRANÇA PARA TÍTULO VENCIDO ANTES DO VENCIMENTO';
         35: Result := 'SOLICITAÇÃO DE CANCELAMENTO DE INSTRUÇÃO INEXISTENTE';
         36: Result := 'TÍTULO SOFRENDO ALTERAÇÃO DE CONTROLE (AGÊNCIA/CONTA/CARTEIRA/NOSSO NÚMERO)';
         37: Result := 'INSTRUÇÃO NÃO PERMITIDA PARA A CARTEIRA';
         38: Result := 'INSTRUÇÃO NÃO PERMITIDA PARA TÍTULO COM RATEIO DE CRÉDITO';
         40: Result := 'INSTRUÇÃO INCOMPATÍVEL – NÃO EXISTE INSTRUÇÃO DE NEGATIVAÇÃO EXPRESSA PARA O TÍTULO';
         41: Result := 'INSTRUÇÃO NÃO PERMITIDA – TÍTULO COM ENTRADA EM NEGATIVAÇÃO EXPRESSA';
         42: Result := 'INSTRUÇÃO NÃO PERMITIDA – TÍTULO COM NEGATIVAÇÃO EXPRESSA CONCLUÍDA';
         43: Result := 'PRAZO INVÁLIDO PARA NEGATIVAÇÃO EXPRESSA – MÍNIMO: 02 DIAS CORRIDOS APÓS O VENCIMENTO';
         45: Result := 'INSTRUÇÃO INCOMPATÍVEL PARA O MESMO TÍTULO NESTA DATA';
         47: Result := 'INSTRUÇÃO NÃO PERMITIDA – ESPÉCIE INVÁLIDA';
         48: Result := 'DADOS DO PAGADOR INVÁLIDOS ( CPF / CNPJ / NOME )';
         49: Result := 'DADOS DO ENDEREÇO DO PAGADOR INVÁLIDOS';
         50: Result := 'DATA DE EMISSÃO DO TÍTULO INVÁLIDA';
         51: Result := 'INSTRUÇÃO NÃO PERMITIDA – TÍTULO COM NEGATIVAÇÃO EXPRESSA AGENDADA';
      else
         Result := IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

      //Tabela 4
      toRetornoBaixaRejeitada:
      case CodMotivo of
         01: Result := 'CARTEIRA/Nº NÚMERO NÃO NUMÉRICO';
         04: Result := 'NOSSO NÚMERO EM DUPLICIDADE NUM MESMO MOVIMENTO';
         05: Result := 'SOLICITAÇÃO DE BAIXA PARA TÍTULO JÁ BAIXADO OU LIQUIDADO';
         06: Result := 'SOLICITAÇÃO DE BAIXA PARA TÍTULO NÃO REGISTRADO NO SISTEMA';
         07: Result := 'COBRANÇA PRAZO CURTO - SOLICITAÇÃO DE BAIXA P/ TÍTULO NÃO REGISTRADO NO SISTEMA';
         08: Result := 'SOLICITAÇÃO DE BAIXA PARA TÍTULO EM FLOATING';
         10: Result := 'VALOR DO TITULO FAZ PARTE DE GARANTIA DE EMPRESTIMO';
         11: Result := 'PAGO ATRAVÉS DO SISPAG POR CRÉDITO EM C/C E NÃO BAIXADO';
      else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

      //Tabela 5
      toRetornoCobrancaContratual:
         case CodMotivo of
            16: Result:= 'ABATIMENTO/ALTERAÇÃO DO VALOR DO TÍTULO OU SOLICITAÇÃO DE BAIXA BLOQUEADOS';
            40: Result:= 'NÃO APROVADA DEVIDO AO IMPACTO NA ELEGIBILIDADE DE GARANTIAS';
            41: Result:= 'AUTOMATICAMENTE REJEITADA';
            42: Result:= 'CONFIRMA RECEBIMENTO DE INSTRUÇÃO – PENDENTE DE ANÁLISE';
         else
            Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
         end;

      //Tabela 6
      toRetornoAlegacaoDoSacado:
      case CodMotivo of
         1313: Result := 'SOLICITA A PRORROGAÇÃO DO VENCIMENTO PARA';
         1321: Result := 'SOLICITA A DISPENSA DOS JUROS DE MORA';
         1339: Result := 'NÃO RECEBEU A MERCADORIA';
         1347: Result := 'A MERCADORIA CHEGOU ATRASADA';
         1354: Result := 'A MERCADORIA CHEGOU AVARIADA';
         1362: Result := 'A MERCADORIA CHEGOU INCOMPLETA';
         1370: Result := 'A MERCADORIA NÃO CONFERE COM O PEDIDO';
         1388: Result := 'A MERCADORIA ESTÁ À DISPOSIÇÃO';
         1396: Result := 'DEVOLVEU A MERCADORIA';
         1404: Result := 'NÃO RECEBEU A FATURA';
         1412: Result := 'A FATURA ESTÁ EM DESACORDO COM A NOTA FISCAL';
         1420: Result := 'O PEDIDO DE COMPRA FOI CANCELADO';
         1438: Result := 'A DUPLICATA FOI CANCELADA';
         1446: Result := 'QUE NADA DEVE OU COMPROU';
         1453: Result := 'QUE MANTÉM ENTENDIMENTOS COM O SACADOR';
         1461: Result := 'QUE PAGARÁ O TÍTULO EM:';
         1479: Result := 'QUE PAGOU O TÍTULO DIRETAMENTE AO CEDENTE EM:';
         1487: Result := 'QUE PAGARÁ O TÍTULO DIRETAMENTE AO CEDENTE EM:';
         1495: Result := 'QUE O VENCIMENTO CORRETO É:';
         1503: Result := 'QUE TEM DESCONTO OU ABATIMENTO DE:';
         1719: Result := 'SACADO NÃO FOI LOCALIZADO; CONFIRMAR ENDEREÇO';
         1727: Result := 'SACADO ESTÁ EM REGIME DE CONCORDATA';
         1735: Result := 'SACADO ESTÁ EM REGIME DE FALÊNCIA';
         1750: Result := 'SACADO SE RECUSA A PAGAR JUROS BANCÁRIOS';
         1768: Result := 'SACADO SE RECUSA A PAGAR COMISSÃO DE PERMANÊNCIA';
         1776: Result := 'NÃO FOI POSSÍVEL A ENTREGA DO BLOQUETO AO SACADO';
         1784: Result := 'BLOQUETO NÃO ENTREGUE, MUDOU-SE/DESCONHECIDO';
         1792: Result := 'BLOQUETO NÃO ENTREGUE, CEP ERRADO/INCOMPLETO';
         1800: Result := 'BLOQUETO NÃO ENTREGUE, NÚMERO NÃO EXISTE/ENDEREÇO INCOMPLETO';
         1818: Result := 'BLOQUETO NÃO RETIRADO PELO SACADO. REENVIADO PELO CORREIO';
         1826: Result := 'ENDEREÇO DE E-MAIL INVÁLIDO. BLOQUETO ENVIADO PELO CORREIO';
         1834: Result := 'BOLETO DDA, DIVIDA RECONHECIDA PELO PAGADOR';
         1842: Result := 'BOLETO DDA, DIVIDA NÃO RECONHECIDA PELO PAGADOR';
      else
         Result := IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

      //Tabela 7
      toRetornoInstrucaoProtestoRejeitadaSustadaOuPendente:
      case CodMotivo of
         1610: Result := 'DOCUMENTAÇÃO SOLICITADA AO CEDENTE';
         3103: Result := 'INSUFICIENCIA DE DADOS NO MODELO 4006';
         3111: Result := 'SUSTAÇÃO SOLICITADA AG. CEDENTE';
         3129: Result := 'TITULO NAO ENVIADO A CARTORIO';
         3137: Result := 'AGUARDAR UM DIA UTIL APOS O VENCTO PARA PROTESTAR';
         3145: Result := 'DM/DMI SEM COMPROVANTE AUTENTICADO OU DECLARACAO';
         3152: Result := 'FALTA CONTRATO DE SERV(AG.CED:ENVIAR)';
         3160: Result := 'NOME DO PAGADOR INCOMPLETO/INCORRETO';
         3178: Result := 'NOME DO BENEFICIÁRIO INCOMPLETO/INCORRETO';
         3186: Result := 'NOME DO SACADOR INCOMPLETO/INCORRETO';
         3194: Result := 'TIT ACEITO: IDENTIF ASSINANTE DO CHEQ';
         3202: Result := 'TIT ACEITO: RASURADO OU RASGADO';
         3210: Result := 'TIT ACEITO: FALTA TIT.(AG.CED:ENVIAR)';
         3228: Result := 'ATOS DA CORREGEDORIA ESTADUAL';
         3236: Result := 'NAO FOI POSSIVEL EFETUAR O PROTESTO';
         3244: Result := 'PROTESTO SUSTADO / CEDENTE NÃO ENTREGOU A DOCUMENTAÇÃO';
         3251: Result := 'DOCUMENTACAO IRREGULAR';
         3269: Result := 'DATA DE EMISSÃO DO TÍTULO INVÁLIDA/IRREGULAR';
         3277: Result := 'ESPECIE INVALIDA PARA PROTESTO';
         3285: Result := 'PRAÇA NÃO ATENDIDA PELA REDE BANCÁRIA';
         3293: Result := 'CENTRALIZADORA DE PROTESTO NAO RECEBEU A DOCUMENTACAO';
         3301: Result := 'CGC/CPF DO SACADO INVÁLIDO/INCORRETO';
         3319: Result := 'SACADOR/AVALISTA E PESSOA FÍSICA';
         3327: Result := 'CEP DO SACADO INCORRETO';
         3335: Result := 'DEPOSITÁRIA INCOMPATÍVEL COM CEP DO SACADO';
         3343: Result := 'CGC/CPF SACADOR INVALIDO/INCORRETO';
         3350: Result := 'ENDEREÇO DO SACADO INSUFICIENTE';
         3368: Result := 'PRAÇA PAGTO INCOMPATÍVEL COM ENDEREÇO';
         3376: Result := 'FALTA NÚMERO/ESPÉCIE DO TÍTULO';
         3384: Result := 'TÍTULO ACEITO S/ ASSINATURA DO SACADOR';
         3392: Result := 'TÍTULO ACEITO S/ ENDOSSO CEDENTE OU IRREGULAR';
         3400: Result := 'TÍTULO SEM LOCAL OU DATA DE EMISSÃO';
         3418: Result := 'TÍTULO ACEITO COM VALOR EXTENSO DIFERENTE DO NUMÉRICO';
         3426: Result := 'TÍTULO ACEITO DEFINIR ESPÉCIE DA DUPLICATA';
         3434: Result := 'DATA EMISSÃO POSTERIOR AO VENCIMENTO';
         3442: Result := 'TÍTULO ACEITO DOCUMENTO NÃO PROSTESTÁVEL';
         3459: Result := 'TÍTULO ACEITO EXTENSO VENCIMENTO IRREGULAR';
         3467: Result := 'TÍTULO ACEITO FALTA NOME FAVORECIDO';
         3475: Result := 'TÍTULO ACEITO FALTA PRAÇA DE PAGAMENTO';
         3483: Result := 'TÍTULO ACEITO FALTA CPF ASSINANTE CHEQUE';
         3491: Result := 'FALTA NÚMERO DO TÍTULO (SEU NÚMERO)';
         3509: Result := 'CARTÓRIO DA PRAÇA COM ATIVIDADE SUSPENSA';
         3517: Result := 'DATA APRESENTACAO MENOR QUE A DATA VENCIMENTO';
         3525: Result := 'FALTA COMPROVANTE DA PRESTACAO DE SERVICO';
         3533: Result := 'CNPJ/CPF PAGADOR INCOMPATIVEL C/ TIPO DE DOCUMENTO';
         3541: Result := 'CNPJ/CPF SACADOR INCOMPATIVEL C/ ESPECIE';
         3558: Result := 'TIT ACEITO: S/ ASSINATURA DO PAGADOR';
         3566: Result := 'FALTA DATA DE EMISSAO DO TITULO';
         3574: Result := 'SALDO MAIOR QUE O VALOR DO TITULO';
         3582: Result := 'TIPO DE ENDOSSO INVALIDO';
         3590: Result := 'DEVOLVIDO POR ORDEM JUDICIAL';
         3608: Result := 'DADOS DO TITULO NAO CONFEREM COM DISQUETE';
         3616: Result := 'PAGADOR E SACADOR AVALISTA SÃO A MESMA PESSOA';
         3624: Result := 'COMPROVANTE ILEGIVEL PARA CONFERENCIA E MICROFILMAGEM';
         3632: Result := 'CONFIRMAR SE SAO DOIS EMITENTES';
         3640: Result := 'ENDERECO DO PAGADOR IGUAL AO DO SACADOR OU DO PORTADOR';
         3657: Result := 'ENDERECO DO BENEFICIÁRIO INCOMPLETO OU NAO INFORMADO';
         3665: Result := 'ENDERECO DO EMITENTE NO CHEQUE IGUAL AO DO BANCO PAGADOR';
         3673: Result := 'FALTA MOTIVO DA DEVOLUCAO NO CHEQUE OU ILEGIVEL';
         3681: Result := 'TITULO COM DIREITO DE REGRESSO VENCIDO';
         3699: Result := 'TITULO APRESENTADO EM DUPLICIDADE';
         3707: Result := 'LC EMITIDA MANUALMENTE (TITULO DO BANCO/CA)';
         3715: Result := 'NAO PROTESTAR LC (TITULO DO BANCO/CA)';
         3723: Result := 'ELIMINAR O PROTESTO DA LC (TITULO DO BANCO/CA)';
         3731: Result := 'TITULO JA PROTESTADO';
         3749: Result := 'TITULO - FALTA TRADUCAO POR TRADUTOR PUBLICO';
         3756: Result := 'FALTA DECLARACAO DE SALDO ASSINADA NO TITULO';
         3764: Result := 'CONTRATO DE CAMBIO - FALTA CONTA GRAFICA';
         3772: Result := 'PAGADOR FALECIDO';
         3780: Result := 'ESPECIE DE TITULO QUE O BANCO NAO PROTESTA';
         3798: Result := 'AUSENCIA DO DOCUMENTO FISICO';
         3806: Result := 'ORDEM DE PROTESTO SUSTADA, MOTIVO';
         3814: Result := 'PAGADOR APRESENTOU QUITAÇÃO DO TÍTULO';
         3822: Result := 'PAGADOR IRÁ NEGOCIAR COM BENEFICIÁRIO';
         3830: Result := 'CPF INCOMPATÍVEL COM A ESPÉCIE DO TÍTULO';
         3848: Result := 'TÍTULO DE OUTRA JURISDIÇÃO TERRITORIAL';
         3855: Result := 'TÍTULO COM EMISSÃO ANTERIOR A CONCORDATA DO PAGADOR';
         3863: Result := 'PAGADOR CONSTA NA LISTA DE FALÊNCIA';
         3871: Result := 'APRESENTANTE NÃO ACEITA PUBLICAÇÃO DE EDITAL';
         3889: Result := 'CARTÓRIO COM PROBLEMAS OPERACIONAIS';
         3897: Result := 'ENVIO DE TITULOS PARA PROTESTO TEMPORARIAMENTE PARALISADO';
         3905: Result := 'BENEFICIÁRIO COM CONTA EM COBRANCA SUSPENSA';
         3913: Result := 'CEP DO PAGADOR É UMA CAIXA POSTAL';
         3921: Result := 'ESPÉCIE NÃO PROTESTÁVEL NO ESTADO';
         3939: Result := 'FALTA ENDEREÇO OU DOCUMENTO DO SACADOR AVALISTA';
         3947: Result := 'CORRIGIR A ESPECIE DO TITULO';
         3954: Result := 'ERRO DE PREENCHIMENTO DO TITULO';
         3962: Result := 'VALOR DIVERGENTE ENTRE TITULO E COMPROVANTE';
         3970: Result := 'CONDOMINIO NAO PODE SER PROTESTADO P/ FINS FALIMENTARES';
         3988: Result := 'VEDADA INTIMACAO POR EDITAL PARA PROTESTO FALIMENTAR';
      else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

      //Tabela 8
      toRetornoInstrucaoCancelada:
      case CodMotivo of
         1156: Result := 'NÃO PROTESTAR';
         2261: Result := 'DISPENSAR JUROS/COMISSÃO DE PERMANÊNCIA';
      else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

      //Tabela 9
      toRetornoChequeDevolvido:
      case CodMotivo of
         11: Result:= 'CHEQUE SEM FUNDOS - PRIMEIRA APRESENTAÇÃO - PASSÍVEL DE REAPRESENTAÇÃO: SIM';
         12: Result:= 'CHEQUE SEM FUNDOS - SEGUNDA APRESENTAÇÃO - PASSÍVEL DE REAPRESENTAÇÃO: NÃO ';
         13: Result:= 'CONTA ENCERRADA - PASSÍVEL DE REAPRESENTAÇÃO: NÃO';
         14: Result:= 'PRÁTICA ESPÚRIA - PASSÍVEL DE REAPRESENTAÇÃO: NÃO';
         20: Result:= 'FOLHA DE CHEQUE CANCELADA POR SOLICITAÇÃO DO CORRENTISTA - PASSÍVEL DE REAPRESENTAÇÃO: NÃO';
         21: Result:= 'CONTRA-ORDEM (OU REVOGAÇÃO) OU OPOSIÇÃO (OU SUSTAÇÃO) AO PAGAMENTO PELO EMITENTE OU PELO ' +
                      'PORTADOR - PASSÍVEL DE REAPRESENTAÇÃO: SIM';
         22: Result:= 'DIVERGÊNCIA OU INSUFICIÊNCIA DE ASSINATURAb - PASSÍVEL DE REAPRESENTAÇÃO: SIM';
         23: Result:= 'CHEQUES EMITIDOS POR ENTIDADES E ÓRGÃOS DA ADMINISTRAÇÃO PÚBLICA FEDERAL DIRETA E INDIRETA, ' +
                      'EM DESACORDO COM OS REQUISITOS CONSTANTES DO ARTIGO 74, § 2º, DO DECRETO-LEI Nº 200, DE 25.02.1967. - ' +
                      'PASSÍVEL DE REAPRESENTAÇÃO: SIM';
         24: Result:= 'BLOQUEIO JUDICIAL OU DETERMINAÇÃO DO BANCO CENTRAL DO BRASIL - PASSÍVEL DE REAPRESENTAÇÃO: SIM';
         25: Result:= 'CANCELAMENTO DE TALONÁRIO PELO BANCO SACADO - PASSÍVEL DE REAPRESENTAÇÃO: NÃO';
         28: Result:= 'CONTRA-ORDEM (OU REVOGAÇÃO) OU OPOSIÇÃO (OU SUSTAÇÃO) AO PAGAMENTO OCASIONADA POR FURTO OU ROUBO - ' +
                      'PASSÍVEL DE REAPRESENTAÇÃO: NÃO';
         29: Result:= 'CHEQUE BLOQUEADO POR FALTA DE CONFIRMAÇÃO DO RECEBIMENTO DO TALONÁRIO PELO CORRENTISTA - ' +
                      'PASSÍVEL DE REAPRESENTAÇÃO: SIM';
         30: Result:= 'FURTO OU ROUBO DE MALOTES - PASSÍVEL DE REAPRESENTAÇÃO: NÃO';
         31: Result:= 'ERRO FORMAL (SEM DATA DE EMISSÃO, COM O MÊS GRAFADO NUMERICAMENTE, AUSÊNCIA DE ASSINATURA, ' +
                      'NÃO-REGISTRO DO VALOR POR EXTENSO) - PASSÍVEL DE REAPRESENTAÇÃO: SIM';
         32: Result:= 'AUSÊNCIA OU IRREGULARIDADE NA APLICAÇÃO DO CARIMBO DE COMPENSAÇÃO - PASSÍVEL DE REAPRESENTAÇÃO: SIM';
         33: Result:= 'DIVERGÊNCIA DE ENDOSSO - PASSÍVEL DE REAPRESENTAÇÃO: SIM';
         34: Result:= 'CHEQUE APRESENTADO POR ESTABELECIMENTO BANCÁRIO QUE NÃO O INDICADO NO CRUZAMENTO EM PRETO, SEM O ' +
                      'ENDOSSO-MANDATO - PASSÍVEL DE REAPRESENTAÇÃO: SIM';
         35: Result:= 'CHEQUE FRAUDADO, EMITIDO SEM PRÉVIO CONTROLE OU RESPONSABILIDADE DO ESTABELECIMENTO BANCÁRIO ' +
                      '("CHEQUE UNIVERSAL"), OU AINDA COM ADULTERAÇÃO DA PRAÇA SACADA - PASSÍVEL DE REAPRESENTAÇÃO: NÃO';
         36: Result:= 'CHEQUE EMITIDO COM MAIS DE UM ENDOSSO - PASSÍVEL DE REAPRESENTAÇÃO: SIM';
         40: Result:= 'MOEDA INVÁLIDA - PASSÍVEL DE REAPRESENTAÇÃO: NÃO';
         41: Result:= 'CHEQUE APRESENTADO A BANCO QUE NÃO O SACADO - PASSÍVEL DE REAPRESENTAÇÃO: SIM';
         42: Result:= 'CHEQUE NÃO-COMPENSÁVEL NA SESSÃO OU SISTEMA DE COMPENSAÇÃO EM QUE FOI APRESENTADO - ' +
                      'PASSÍVEL DE REAPRESENTAÇÃO: SIM';
         43: Result:= 'CHEQUE, DEVOLVIDO ANTERIORMENTE PELOS MOTIVOS 21, 22, 23, 24, 31 OU 34, NÃO-PASSÍVEL ' +
                      'DE REAPRESENTAÇÃO EM VIRTUDE DE PERSISTIR O MOTIVO DA DEVOLUÇÃO - PASSÍVEL DE REAPRESENTAÇÃO: NÃO';
         44: Result:= 'CHEQUE PRESCRITO - PASSÍVEL DE REAPRESENTAÇÃO: NÃO';
         45: Result:= 'CHEQUE EMITIDO POR ENTIDADE OBRIGADA A REALIZAR MOVIMENTAÇÃO E UTILIZAÇÃO DE RECURSOS FINANCEIROS ' +
                      'DO TESOURO NACIONAL MEDIANTE ORDEM BANCÁRIA - PASSÍVEL DE REAPRESENTAÇÃO: NÃO';
         48: Result:= 'CHEQUE DE VALOR SUPERIOR AO ESTABELECIDO, EMITIDO SEM A IDENTIFICAÇÃO DO BENEFICIÁRIO, DEVENDO SER ' +
                      'DEVOLVIDO A QUALQUER TEMPO - PASSÍVEL DE REAPRESENTAÇÃO: SIM';
         49: Result:= 'REMESSA NULA, CARACTERIZADA PELA REAPRESENTAÇÃO DE CHEQUE DEVOLVIDO PELOS MOTIVOS 12, 13, 14, 20, ' +
                      '25, 28, 30, 35, 43, 44 E 45, PODENDO A SUA DEVOLUÇÃO OCORRER A QUALQUER TEMPO - PASSÍVEL DE REAPRESENTAÇÃO: NÃO';
      else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

      // Tabela 10
      toRetornoRegistroConfirmado:
      case CodMotivo of
        01: Result := 'CEP SEM ATENDIMENTO DE PROTESTO NO MOMENTO';
        02: Result := 'ESTADO COM DETERMINAÇÃO LEGAL QU EIMPEDE A INSCRIÇÃO DE INADIMPLENTES NOS CADASTROS DE PROTEÇÃO AO ' +
                      'CRÉDITO NO PRAZO SOLICITADO – PRAZO SUPERIOR AO SOLICITADO';
        03: Result := 'BOLETO NÃO LIQUIDADO NO DESCONTO DE DUPLICATAS E TRANSFERIDO PARA COBRANÇA SIMPLES';
      else
        Result:= IntToStrZero(CodMotivo, 2) + ' - Outros Motivos';
      end;

      // Tabela 11
      toRetornoInstrucaoNegativacaoExpressaRejeitada:
      case CodMotivo of
        6007: Result := 'INCLUSÃO BLOQUEADA FACE A DETERMINAÇÃO JUDICIAL';
        6015: Result := 'INCONSISTÊNCIAS NAS INFORMAÇÕES DE ENDEREÇO';
        6023: Result := 'TÍTULO JÁ DECURSADO';
        6031: Result := 'INCLUSÃO CONDICIONADA A APRESENTAÇÃO DE DOCUMENTO DE DÍVIDA';
        6163: Result := 'EXCLUSÃO NÃO PERMITIDA, REGISTRO SUSPENSO';
        6171: Result := 'EXCLUSÃO PARA REGISTRO INEXISTENTE';
        6379: Result := 'REJEIÇÃO POR DADO(S) INCONSISTENTE(S)';
      else
        Result:= IntToStrZero(CodMotivo, 2) + ' - Outros Motivos';
      end;

      // Tabela 12
      toRetornoNegativacaoExpressaInformacional:
      case CodMotivo of
        6049: Result := 'INFORMAÇÃO DOS CORREIOS – MUDOU-SE';
        6056: Result := 'INFORMAÇÃO DOS CORREIOS – DEVOLVIDO POR INFORMAÇÃO PRESTADA PELO SINDICO OU PORTEIRO';
        6064: Result := 'INFORMAÇÃO DOS CORREIOS – DEVOLVIDO POR INCONSISTÊNCIA NO ENDEREÇO';
        6072: Result := 'INFORMAÇÃO DOS CORREIOS – DESCONHECIDO';
        6080: Result := 'INFORMAÇÃO DOS CORREIOS – RECUSADO';
        6098: Result := 'INFORMAÇÃO DOS CORREIOS – AUSENTE';
        6106: Result := 'INFORMAÇÃO DOS CORREIOS – NÃO PROCURADO';
        6114: Result := 'INFORMAÇÃO DOS CORREIOS – FALECIDO';
        6122: Result := 'INFORMAÇÃO DOS CORREIOS – NÃO ESPECIFICADO';
        6130: Result := 'INFORMAÇÃO DOS CORREIOS – CAIXA POSTAL INEXISTENTE';
        6148: Result := 'INFORMAÇÃO DOS CORREIOS – DEVOLUÇÃO DO COMUNICADO DO CORREIO';
        6155: Result := 'INFORMAÇÃO DOS CORREIOS – OUTROS MOTIVOS';
        6478: Result := 'AR - ENTREGUE COM SUCESSO';
        6486: Result := 'INCLUSAO PARA REGISTRO JA EXISTENTE/RECUSADO';
        6494: Result := 'AR - CARTA EXTRAVIADA E NÃO ENTREGUE';
        6502: Result := 'AR - CARTA ROUBADA E NÃO ENTREGUE';
        6510: Result := 'AR - AUSENTE - ENCAMINHADO PARA ENTREGA INTERNA';
        6528: Result := 'AR INUTILIZADO - NÃO RETIRADO NOS CORREIOS APÓS 3 TENTATIVAS';
        6536: Result := 'AR - ENDERECO INCORRETO';
        6544: Result := 'AR - NAO PROCURADO – DEVOLVIDO AO REMETENTE';
        6551: Result := 'AR - NÃO ENTREGUE POR FALTA DE APRESENTAR DOCUMENTO COM FOTO';
        6569: Result := 'AR - MUDOU-SE';
        6577: Result := 'AR - DESCONHECIDO';
        6585: Result := 'AR - RECUSADO';
        6593: Result := 'AR - ENDERECO INSUFICIENTE';
        6601: Result := 'AR - NAO EXISTE O NUMERO INDICADO';
        6618: Result := 'AR – AUSENTE';
        6627: Result := 'AR - CARTA NAO PROCURADA NA UNIDADE DOS CORREIOS';
        6635: Result := 'AR – FALECIDO';
        6643: Result := 'AR - DEVIDO A DEVOLUCAO DO COMUNICADO DO CORREIO';
      else
        Result:= IntToStrZero(CodMotivo, 2) + ' - Outros Motivos';
      end;
   else
      Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
   end;

  Result := ACBrSTr(Result);
end;

function TACBrBancoItau.CodOcorrenciaToTipoRemessa(const CodOcorrencia:Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02 : Result:= toRemessaBaixar;                          {Pedido de Baixa}
    04 : Result:= toRemessaConcederAbatimento;              {Concessão de Abatimento}
    05 : Result:= toRemessaCancelarAbatimento;              {Cancelamento de Abatimento concedido}
    06 : Result:= toRemessaAlterarVencimento;               {Alteração de vencimento}
    07 : Result:= toRemessaAlterarUsoEmpresa;               {Alteração do uso Da Empresa}
    08 : Result:= toRemessaAlterarSeuNumero;                {Alteração do seu Número}
    09 : Result:= toRemessaProtestar;                       {Protestar (emite aviso ao sacado após xx dias do vencimento, e envia ao cartório após 5 dias úteis)}
    10 : Result:= toRemessaCancelarInstrucaoProtesto;       {Sustar Protesto}
    11 : Result:= toRemessaProtestoFinsFalimentares;        {Protesto para fins Falimentares}
    18 : Result:= toRemessaCancelarInstrucaoProtestoBaixa;  {Sustar protesto e baixar}
    30 : Result:= toRemessaExcluirSacadorAvalista;          {Exclusão de Sacador Avalista}
    31 : Result:= toRemessaOutrasAlteracoes;                {Alteração de Outros Dados}
    34 : Result:= toRemessaBaixaporPagtoDiretoCedente;      {Baixa por ter sido pago Diretamente ao Cedente}
    35 : Result:= toRemessaCancelarInstrucao;               {Cancelamento de Instrução}
    37 : Result:= toRemessaAlterarVencimentoSustarProtesto; {Alteração do Vencimento e Sustar Protesto}
    38 : Result:= toRemessaCedenteDiscordaSacado;           {Cedente não Concorda com Alegação do Sacado }
    47 : Result:= toRemessaCedenteSolicitaDispensaJuros;    {Cedente Solicita Dispensa de Juros}
    49 : Result:= toRemessaAlterarOutrosDados;              {49-Alteração de dados extras}
    71 : Result:= toRemessaHibrido                          {71-REMESSA – BOLECODE (emissão do boleto e QR Code pix)}
  else
     Result:= toRemessaRegistrar;                           {Remessa}
  end;

end;

function TACBrBancoItau.CodigoLiquidacao_Descricao(CodLiquidacao: String): String;
begin
  CodLiquidacao := Trim(CodLiquidacao);
  if AnsiSameText(CodLiquidacao, 'AA') then
    Result := 'CAIXA ELETRÔNICO BANCO ITAÚ'
  else if AnsiSameText(CodLiquidacao, 'AC') then
    Result := 'PAGAMENTO EM CARTÓRIO AUTOMATIZADO'
  else if AnsiSameText(CodLiquidacao, 'AO') then
    Result := 'ACERTO ONLINE'
  else if AnsiSameText(CodLiquidacao, 'BC') then
    Result := 'BANCOS CORRESPONDENTES'
  else if AnsiSameText(CodLiquidacao, 'BF') then
    Result := 'ITAÚ BANKFONE'
  else if AnsiSameText(CodLiquidacao, 'BL') then
    Result := 'ITAÚ BANKLINE'
  else if AnsiSameText(CodLiquidacao, 'B0') then
    Result := 'OUTROS BANCOS – RECEBIMENTO OFF-LINE'
  else if AnsiSameText(CodLiquidacao, 'B1') then
    Result := 'OUTROS BANCOS – PELO CÓDIGO DE BARRAS'
  else if AnsiSameText(CodLiquidacao, 'B2') then
    Result := 'OUTROS BANCOS – PELA LINHA DIGITÁVEL'
  else if AnsiSameText(CodLiquidacao, 'B3') then
    Result := 'OUTROS BANCOS – PELO AUTO ATENDIMENTO'
  else if AnsiSameText(CodLiquidacao, 'B4') then
    Result := 'OUTROS BANCOS – RECEBIMENTO EM CASA LOTÉRICA'
  else if AnsiSameText(CodLiquidacao, 'B5') then
    Result := 'OUTROS BANCOS – CORRESPONDENTE'
  else if AnsiSameText(CodLiquidacao, 'B6') then
    Result := 'OUTROS BANCOS – TELEFONE'
  else if AnsiSameText(CodLiquidacao, 'B7') then
    Result := 'OUTROS BANCOS – ARQUIVO ELETRÔNICO'
  else if AnsiSameText(CodLiquidacao, 'CC') then
    Result := 'AGÊNCIA ITAÚ – COM CHEQUE DE OUTRO BANCO ou (CHEQUE ITAÚ)'
  else if AnsiSameText(CodLiquidacao, 'CI') then
    Result := 'CORRESPONDENTE ITAÚ'
  else if AnsiSameText(CodLiquidacao, 'CK') then
    Result := 'SISPAG – SISTEMA DE CONTAS A PAGAR ITAÚ'
  else if AnsiSameText(CodLiquidacao, 'CP') then
    Result := 'AGÊNCIA ITAÚ – POR DÉBITO EM CONTA CORRENTE, CHEQUE ITAÚ OU DINHEIRO'
  else if AnsiSameText(CodLiquidacao, 'DG') then
    Result := 'AGÊNCIA ITAÚ – CAPTURADO EM OFF-LINE'
  else if AnsiSameText(CodLiquidacao, 'LC') then
    Result := 'PAGAMENTO EM CARTÓRIO DE PROTESTO COM CHEQUE'
  else if AnsiSameText(CodLiquidacao, 'EA') then
    Result := 'TERMINAL DE CAIXA'
  else if AnsiSameText(CodLiquidacao, 'Q0') then
    Result := 'AGENDAMENTO – PAGAMENTO AGENDADO VIA BANKLINE OU OUTRO CANAL ELETRÔNICO E LIQUIDADO NA DATA INDICADA'
  else if AnsiSameText(CodLiquidacao, 'RA') then
    Result := 'DIGITAÇÃO – REALIMENTAÇÃO AUTOMÁTICA'
  else if AnsiSameText(CodLiquidacao, 'ST') then
    Result := 'PAGAMENTO VIA SELTEC'
  else
    Result := '';
end;

function TACBrBancoItau.TipoOcorrenciaToCodRemessa(
  const TipoOcorrencia: TACBrTipoOcorrencia): String;
begin
  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
  begin
    case TipoOcorrencia of
      toRemessaBaixar                    : Result := '02';
      toRemessaConcederAbatimento        : Result := '04';
      toRemessaCancelarAbatimento        : Result := '05';
      toRemessaAlterarVencimento         : Result := '06';
      toRemessaSustarProtesto            : Result := '18';
      toRemessaCancelarInstrucaoProtesto : Result := '10';
    else
       Result := '01';
    end;
  end
  else
  begin
    case TipoOcorrencia of
      toRemessaBaixar                       : Result := '02';
      toRemessaConcederAbatimento           : Result := '04';
      toRemessaCancelarAbatimento           : Result := '05';
      toRemessaAlterarVencimento            : Result := '06';
      toRemessaAlterarUsoEmpresa            : Result := '07';
      toRemessaAlterarSeuNumero             : Result := '08';
      toRemessaProtestar                    : Result := '09';
      toRemessaNaoProtestar                 : Result := '10';
      toRemessaProtestoFinsFalimentares     : Result := '11';
      toRemessaSustarProtesto               : Result := '18';
      toRemessaOutrasAlteracoes             : Result := '31';
      toRemessaBaixaporPagtoDiretoCedente   : Result := '34';
      toRemessaCancelarInstrucao            : Result := '35';
      toRemessaAlterarVencSustarProtesto    : Result := '37';
      toRemessaCedenteDiscordaSacado        : Result := '38';
      toRemessaCedenteSolicitaDispensaJuros : Result := '47';
      toRemessaHibrido                      : Result := '71';
    else
      Result := '01';
    end;
  end;

end;

end.
