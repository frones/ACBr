{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior, Juliana Tamizou e Daniel }
{ de Morais(InfoCotidiano)                                                     }
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

unit ACBrBancoBrasil;

interface

uses
  Classes, SysUtils, Contnrs,
  ACBrBoleto, ACBrBoletoConversao;

const
  CACBrBancoBrasil_Versao = '0.0.1';

type
  { TACBrBancoBrasil}

  TACBrBancoBrasil = class(TACBrBancoClass)
   protected
     procedure EhObrigatorioContaDV; override;
     procedure EhObrigatorioAgenciaDV; override;
     function FormataNossoNumero(const ACBrTitulo :TACBrTitulo): String;
   private
    fQtMsg: Integer;
    function NossoNumeroSemFormatacaoLerRetorno(const Convenio, Carteira, Linha: String): String;
    procedure LerRetorno400Pos6(ARetorno: TStringList);
    procedure LerRetorno400Pos7(ARetorno: TStringList);
   public
    Constructor create(AOwner: TACBrBanco);
    function CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo ): String; override;
    function MontarCodigoBarras(const ACBrTitulo : TACBrTitulo): String; override;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String; override;
    function MontarCampoCarteira(const ACBrTitulo: TACBrTitulo): String; override;
    function MontarCampoNossoNumero(const ACBrTitulo :TACBrTitulo): String; override;
    function GerarRegistroHeader240(NumeroRemessa : Integer): String; override;
    function GerarRegistroTransacao240(ACBrTitulo : TACBrTitulo): String; override;
    function GerarRegistroTrailler240(ARemessa : TStringList): String;  override;
    procedure GerarRegistroHeader400(NumeroRemessa : Integer; aRemessa:TStringList); override;
    procedure GerarRegistroTransacao400(ACBrTitulo : TACBrTitulo; aRemessa: TStringList); override;
    procedure GerarRegistroTrailler400(ARemessa : TStringList);  override;
    function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia) : String; override;
    function CodOcorrenciaToTipo(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
    function TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia):String; override;
    Procedure LerRetorno240(ARetorno:TStringList); override;
    procedure LerRetorno400(ARetorno: TStringList); override;

    function CodMotivoRejeicaoToDescricao(
      const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: Integer): String; override;

    function CalcularTamMaximoNossoNumero(const Carteira : String; const NossoNumero : String = ''; const Convenio: String = ''): Integer; override;

    function CodOcorrenciaToTipoRemessa(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
   end;

implementation

uses {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF},
  StrUtils, Variants, ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings,
  ACBrUtil.DateTime, Math;

constructor TACBrBancoBrasil.create(AOwner: TACBrBanco);
begin
   inherited create(AOwner);
   fpDigito                := 9;
   fpNome                  := 'Banco do Brasil';
   fpNumero                := 001;
   fpTamanhoMaximoNossoNum := 0;
   fpTamanhoConta          := 12;
   fpTamanhoAgencia        := 4;
   fpTamanhoCarteira       := 2;
   fpCodigosMoraAceitos    := '123';
   fQtMsg                  := 0;
end;

procedure TACBrBancoBrasil.EhObrigatorioAgenciaDV;
begin
  if ACBrBanco.TipoCobranca <> cobBancoDoBrasilAPI then
    inherited;
end;

procedure TACBrBancoBrasil.EhObrigatorioContaDV;
begin
  if ACBrBanco.TipoCobranca <> cobBancoDoBrasilAPI then
    inherited;
end;

function TACBrBancoBrasil.CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo ): String;
begin
   Result := '0';

   Modulo.CalculoPadrao;
   Modulo.MultiplicadorFinal   := 2;
   Modulo.MultiplicadorInicial := 9;
   Modulo.Documento := FormataNossoNumero(ACBrTitulo);
   Modulo.Calcular;

   if Modulo.ModuloFinal >= 10 then
      Result:= 'X'
   else
      Result:= IntToStr(Modulo.ModuloFinal);
end;

function TACBrBancoBrasil.CalcularTamMaximoNossoNumero(
  const Carteira: String; const NossoNumero : String = ''; const Convenio: String = ''): Integer;
var
  wCarteira   : String;
  wTamConvenio: Integer;
begin
   Result := 10;

   if (ACBrBanco.ACBrBoleto.Cedente.Convenio = '') then
      raise Exception.Create(ACBrStr(fpNome + ' requer que o Convênio do Cedente '+
                                     'seja informado.'));

   if (Carteira = '') then
      raise Exception.Create(ACBrStr(fpNome + ' requer que a carteira seja '+
                                     'informada antes do Nosso Número.'));

   wCarteira:= Trim(Carteira);
   wTamConvenio:= Length(Trim(ACBrBanco.ACBrBoleto.Cedente.Convenio));

   if (Length(trim(NossoNumero)) > 10) and
      (((wTamConvenio = 6) and ((wCarteira = '16') or (wCarteira = '18'))) or
      ((wTamConvenio = 7) and ((wCarteira = '17') or (wCarteira = '18')))) then
      Result:= 17
   else if (wTamConvenio <= 4) then
      Result := 7
   else if ((wTamConvenio > 4) and (wTamConvenio < 6)) or
           ((wTamConvenio = 6) and ((wCarteira = '12') or (wCarteira = '15') or
            (wCarteira = '17') or (wCarteira = '18'))) then
      Result := 5
   else if (wTamConvenio = 6) then
      Result := 11
   else if (wTamConvenio = 7) then
      Result := 10;
end;

function TACBrBancoBrasil.FormataNossoNumero(const ACBrTitulo :TACBrTitulo): String;
var
  ANossoNumero, AConvenio: String;
  wTamNossoNum: Integer;
begin
  with ACBrTitulo do
  begin
    AConvenio    := ACBrBoleto.Cedente.Convenio;
    ANossoNumero := NossoNumero;
    wTamNossoNum := CalcularTamMaximoNossoNumero(Carteira,ANossoNumero);

    if ((ACBrTitulo.Carteira = '16') or (ACBrTitulo.Carteira = '18')) and
        (Length(AConvenio) = 6) and (wTamNossoNum = 17) then
      ANossoNumero := PadLeft(ANossoNumero, 17, '0')
    else if Length(AConvenio) <= 4 then
      ANossoNumero := PadLeft(AConvenio, 4, '0') + PadLeft(ANossoNumero, 7, '0')
    else if (Length(AConvenio) > 4) and (Length(AConvenio) <= 6) then
      ANossoNumero := PadLeft(AConvenio, 6, '0') + PadLeft(ANossoNumero, 5, '0')
    else if (Length(AConvenio) = 7) then
      ANossoNumero := PadLeft(AConvenio, 7, '0') + RightStr(ANossoNumero, 10);

    if (ACBrTitulo.ACBrBoleto.Banco.TipoCobranca = cobBancoDoBrasilAPI) then
      ANossoNumero := PadLeft(AConvenio+IntToStr(StrToInt64Def(NossoNumero,0)) , 20, '0');

  end;
  Result := ANossoNumero;
end;

function TACBrBancoBrasil.NossoNumeroSemFormatacaoLerRetorno(const Convenio,
  Carteira, Linha: String): String;
begin

  //Utiliza Mesma Regra da Funcão FormataNossoNumero para Extrair apenas o campo Nosso Número do Retorno
  if ( (Length(Convenio) = 6) and (Length(trim(copy (Linha, 38, 20) )) = 17) )  then
    Result := copy(Linha, 38, 17)  // Utiliza 17 posições correspondente Nosso Numero
  else if ( Length(Convenio) <= 4 ) then
    Result := copy(Linha, 42, 7)  // Elimina 4 posições do Convênio e utiliza 7 posições correspondente Nosso Numero
  else if ( (Length(Convenio) > 4) and (Length(Convenio) <= 6) ) then
    Result := copy(Linha, 44, 5)  // Elimina 6 posições do Convênio e utiliza 5 posições correspondente Nosso Numero
  else if ( Length(Convenio) = 7 ) then
    Result := copy(Linha, 45, 10) // Elimina 7 posições do Convênio e utiliza 10 posições correspondente Nosso Numero
  else
    Result := copy (Linha, 38, 20);

end;


function TACBrBancoBrasil.MontarCodigoBarras(const ACBrTitulo : TACBrTitulo): String;
var
  CodigoBarras, FatorVencimento, DigitoCodBarras :String;
  ANossoNumero, AConvenio: String;
  wTamNossNum: Integer;
begin
   AConvenio    := ACBrTitulo.ACBrBoleto.Cedente.Convenio;
   ANossoNumero := FormataNossoNumero(ACBrTitulo);
   wTamNossNum  := CalcularTamMaximoNossoNumero(ACBrTitulo.Carteira,
                                                ACBrTitulo.NossoNumero);

   {Codigo de Barras}
   with ACBrTitulo.ACBrBoleto do
   begin
      FatorVencimento := CalcularFatorVencimento(ACBrTitulo.Vencimento);

      if (Banco.TipoCobranca = cobBancoDoBrasilAPI) then
        ANossoNumero := Copy(ANossoNumero,4,Length(ANossoNumero));

      if ((ACBrTitulo.Carteira = '18') or (ACBrTitulo.Carteira = '16')) and
         (Length(AConvenio) = 6) and (wTamNossNum = 17) then
       begin
         CodigoBarras := IntToStrZero(Banco.Numero, 3) +
                         '9' +
                         FatorVencimento +
                         IntToStrZero(Round(ACBrTitulo.ValorDocumento * 100), 10) +
                         AConvenio + ANossoNumero + '21';
       end
      else
       begin
         CodigoBarras := IntToStrZero(Banco.Numero, 3) +
                         '9' +
                         FatorVencimento +
                         IntToStrZero(Round(ACBrTitulo.ValorDocumento * 100), 10) +
                         IfThen((Length(AConvenio) = 7), '000000', '') +
                         ANossoNumero +
                         IfThen((Length(AConvenio) < 7), PadLeft(OnlyNumber(Cedente.Agencia), 4, '0'), '') +
                         IfThen((Length(AConvenio) < 7), IntToStrZero(StrToIntDef(OnlyNumber(Cedente.Conta),0),8), '') +
                         ACBrTitulo.Carteira;
       end;


      DigitoCodBarras := CalcularDigitoCodigoBarras(CodigoBarras);
   end;

   Result:= copy( CodigoBarras, 1, 4) + DigitoCodBarras + copy( CodigoBarras, 5, 44) ;
end;

function TACBrBancoBrasil.MontarCampoCodigoCedente (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
  if(ACBrTitulo.ACBrBoleto.Banco.TipoCobranca = cobBancoDoBrasilAPI) then
  begin
    Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia+'/'+
             IntToStr(StrToIntDef(ACBrTitulo.ACBrBoleto.Cedente.Conta,0));
  end else
  begin
    Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia+'-'+
             ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito+'/'+
             IntToStr(StrToIntDef(ACBrTitulo.ACBrBoleto.Cedente.Conta,0)) +'-'+
             ACBrTitulo.ACBrBoleto.Cedente.ContaDigito;
   end;
end;

function TACBrBancoBrasil.MontarCampoCarteira(const ACBrTitulo: TACBrTitulo
  ): String;
begin
  Result := IfThen(ACBrTitulo.ACBrBoleto.Cedente.Modalidade = '',
                   ACBrTitulo.Carteira,
                   ACBrTitulo.Carteira + '/' + ACBrTitulo.ACBrBoleto.Cedente.Modalidade );
end;

function TACBrBancoBrasil.MontarCampoNossoNumero (const ACBrTitulo: TACBrTitulo ) : String;
var
  ANossoNumero :string;
  wTamConvenio, wTamNossoNum :Integer;
begin
   ANossoNumero := FormataNossoNumero(ACBrTitulo);
   wTamConvenio := Length(ACBrBanco.ACBrBoleto.Cedente.Convenio);
   wTamNossoNum := CalcularTamMaximoNossoNumero(ACBrTitulo.Carteira,
                                                OnlyNumber(ACBrTitulo.NossoNumero));

   if ((wTamConvenio = 6) and (wTamNossoNum = 17)) or (wTamConvenio = 7) or (ACBrTitulo.ACBrBoleto.Banco.TipoCobranca = cobBancoDoBrasilAPI)  then
      Result:= ANossoNumero
   else
      Result := ANossoNumero + '-' + CalcularDigitoVerificador(ACBrTitulo);
end;


function TACBrBancoBrasil.GerarRegistroHeader240(NumeroRemessa : Integer): String;
var
  ATipoInscricao, aConta, aAgencia, aModalidade, aCSP, str0: String;
  VersaoArquivo, VersaoLote: Integer;
begin

   with ACBrBanco.ACBrBoleto.Cedente do
   begin
      case TipoInscricao of
         pFisica  : ATipoInscricao := '1';
         pJuridica: ATipoInscricao := '2';
      else
          ATipoInscricao := '1';
      end;

      aAgencia    := PadLeft(OnlyNumber(Agencia), 5, '0');
      aConta      := PadLeft(OnlyNumber(Conta), 12, '0');
      aModalidade := PadLeft(trim(Modalidade), 3, '0');

      VersaoArquivo := LayoutVersaoArquivo;

      if not (VersaoArquivo in [030, 040, 080, 082, 083, 084, 087]) then
        VersaoArquivo := 030;

      {
      Se o arquivo foi formatado com a  versão do layout  030,
      pode ser informado 'CSP' nas posições 223 a 225.
      }

      if VersaoArquivo = 030 then
      begin
        aCSP := 'CSP';
        str0 := '000';
      end 
      else 
      begin
        aCSP := '';
        str0 := '';
      end;

      { GERAR REGISTRO-HEADER DO ARQUIVO }

      Result:= IntToStrZero(ACBrBanco.Numero, 3)               + // 001 a 003 - Código do banco
               '0000'                                          + // 004 a 007 - Lote de serviço
               '0'                                             + // 008 a 008 - Tipo de registro - Registro header de arquivo
               StringOfChar(' ', 9)                            + // 009 a 017 - Uso exclusivo FEBRABAN/CNAB
               ATipoInscricao                                  + // 018 a 018 - Tipo de inscrição do cedente
               PadLeft(OnlyNumber(CNPJCPF), 14, '0')           + // 019 a 032 - Número de inscrição do cedente
               PadLeft(Convenio, 9, '0')                       + // 033 a 041 - Código do convênio no banco [ Alterado conforme instruções da CSO Brasília ] 27-07-09
               '0014'                                          + // 042 a 045 - Informar 0014 Cedente
               ACBrBanco.ACBrBoleto.ListadeBoletos[0].Carteira + // 046 a 047 - Carteira
               aModalidade                                     + // 048 a 050 - Variacao Carteira
               '  '                                            + // 051 a 052 - Informar Brancos
               aAgencia                                        + // 053 a 057 - Código da agência do cedente
               PadRight(AgenciaDigito, 1 , '0')                + // 058 a 058 - Dígito da agência do cedente
               aConta                                          + // 059 a 070 - Número da conta do cedente
               PadRight(ContaDigito, 1, '0')                   + // 071 a 071 - Dígito da conta do cedente
               ' '                                             + // 072 a 072 - Dígito verificador da agência / conta
               TiraAcentos(UpperCase(PadRight(Nome, 30, ' '))) + // 073 a 102 - Nome do cedente
               PadRight(UpperCase(fpNome), 30, ' ')            + // 103 a 132 - Nome do banco
               StringOfChar(' ', 10)                           + // 133 a 142 - Uso exclusivo FEBRABAN/CNAB
               '1'                                             + // 143 a 143 - Código de Remessa (1) / Retorno (2)
               FormatDateTime('ddmmyyyy', Now)                 + // 144 a 151 - Data do de geração do arquivo
               FormatDateTime('hhmmss', Now)                   + // 152 a 157 - Hora de geração do arquivo
               PadLeft(IntToStr(NumeroRemessa), 6, '0')        + // 158 a 163 - Número seqüencial do arquivo
               PadLeft(IntToStr(VersaoArquivo), 3, '0')        + // 164 a 166 - Número da versão do layout do arquivo
               StringOfChar('0', 5)                            + // 167 a 171 - Densidade de gravação do arquivo (BPI)
               StringOfChar(' ', 20)                           + // 172 a 191 - Uso reservado do banco
               StringOfChar('0', 20)                           + // 192 a 211 - Uso reservado da empresa
               StringOfChar(' ', 11)                           + // 212 a 222 - 11 brancos
               PadLeft(aCSP, 3, ' ')                           + // 223 a 225 - Informar 'CSP' se a versão for 030, caso contrario informar branco
               PadLeft(str0, 3, ' ')                           + // 226 a 228 - Uso exclusivo de Vans
               StringOfChar(' ', 2)                            + // 229 a 230 - Tipo de servico
               StringOfChar(' ', 10);                            // 231 a 240 - titulo em carteira de cobranca

          { GERAR REGISTRO HEADER DO LOTE }

      { *** Versao do Layout do Lote ***
      Campo não criticado pelo sistema. Informar Zeros OU se preferir,
      informar número da versão do leiaute do Lote que foi utilizado como base
      para formatação dos campos.
      Versões disponíveis: 043, 042, 041, 040, 030 e 020.
      A versão do Lote quando informada deve estar condizente com a versão do
      Arquivo (posições 164 a 166 do Header de Arquivo).
      }

    //  VersaoLote := LayoutVersaoLote;

      case VersaoArquivo of
        030: VersaoLote := 020;
        040: VersaoLote := 030;
        080: VersaoLote := 040;
        082: VersaoLote := 041;
        083: VersaoLote := 042;
        084: VersaoLote := 043;
        087: VersaoLote := 045;
      else
        VersaoLote := 000;
      end;

      Result:= Result + #13#10 +
               IntToStrZero(ACBrBanco.Numero, 3)                  + // 001 a 003 - Código do banco
               '0001'                                             + // 004 a 007 - Lote de serviço
               '1'                                                + // 008 a 008 Tipo de registro - Registro header de arquivo
               'R'                                                + // 009 a 009 Tipo de operação: R (Remessa) ou T (Retorno)
               '01'                                               + // 010 a 011 - Tipo de serviço: 01 (Cobrança)
               '  '                                               + // 012 a 013 - Forma de lançamento: preencher com ZEROS no caso de cobrança
               PadLeft(IntToStr(VersaoLote), 3, '0')              + // 014 a 016 - Número da versão do layout do lote
               ' '                                                + // 017 a 017 Uso exclusivo FEBRABAN/CNAB
               ATipoInscricao                                     + // 018 a 018 Tipo de inscrição do cedente
               PadLeft(OnlyNumber(CNPJCPF), 15, '0')              + // 019 a 033 -Número de inscrição do cedente
               PadLeft(Convenio, 9, '0')                          + // 034 a 042 Código do convênio no banco
               '0014'                                             + // 043 a 046 Informar 0014 para cobranca cedente
               ACBrBanco.ACBrBoleto.ListadeBoletos[0].Carteira    + // 047 a 048 Carteira cobranca
               aModalidade                                        + // 049 a 051 Variacao carteira
               IfThen(ACBrBanco.ACBrBoleto.Homologacao,'TS','  ') + // 052 a 053 Infrmar TS para Homologacao ou branco para producao
               aAgencia                                           + // 054 a 058 - Código da agência do cedente
               PadRight(AgenciaDigito, 1 , '0')                   + // 059 a 059 Dígito da agência do cedente
               aConta                                             + // 060 a 071 - Número da conta do cedente
               PadRight(ContaDigito, 1, '0')                      + // 072 a 072 Dígito da conta do cedente
               ' '                                                + // 073 a 073 Dígito verificador da agência / conta
               PadRight(Nome, 30, ' ')                            + // 074 a 103 - Nome do cedente
               StringOfChar(' ', 40)                              + // 104 a 143 - Mensagem 1 para todos os boletos do lote
               StringOfChar(' ', 40)                              + // 144 a 183 - Mensagem 2 para todos os boletos do lote
               PadLeft(IntToStr(NumeroRemessa), 8, '0')           + // 184 a 191 - Número do arquivo
               FormatDateTime('ddmmyyyy', Now)                    + // 192 a 199 - Data de geração do arquivo
               StringOfChar('0', 8)                               + // 200 a 207 - Data do crédito - Só para arquivo retorno
               StringOfChar(' ', 33);                               // 208 a 240 - Uso exclusivo FEBRABAN/CNAB
   end;
end;

function TACBrBancoBrasil.GerarRegistroTransacao240(ACBrTitulo : TACBrTitulo): String;
var
   ATipoOcorrencia, ATipoBoleto : String;
   ADataMoraJuros, ADataDesconto, ADataDesconto2, ADataDesconto3: String;
   ACodigoDesconto: String;
   ANossoNumero, ATipoAceite    : String;
   aAgencia, aConta, aDV        : String;
   wTamConvenio, wTamNossoNum   : Integer;
   wCarteira, QtdRegTitulo      : Integer;
   ACaracTitulo, wTipoCarteira  : Char;
   AMensagem                    : String;
   ACodProtesto                 : Char;
   BoletoEmail,GeraSegS         : Boolean;
   DataProtestoNegativacao      : string;
   DiasProtestoNegativacao      : string;
   ATipoDocumento               : String;
   sDiasBaixa                   : String;

  function MontarInstrucoes2: string;
  begin
    Result := '';

    with ACBrTitulo do
    begin
       Result:= PadRight(Mensagem[1], 40, ' ');

       if Mensagem.Count > 2 then
          Result:= Result + PadRight(Mensagem[2], 40, ' ')
       else
          exit;

       if Mensagem.Count > 3 then
          Result:= Result + PadRight(Mensagem[3], 40, ' ')
       else
         exit;

       if Mensagem.Count > 4 then
          Result:= Result + PadRight(Mensagem[4], 40, ' ')
       else
          exit;

       if Mensagem.Count > 5 then
          Result:= Result + PadRight(Mensagem[5], 40, ' ')
       else
          exit;
    end;
  end;

begin
   with ACBrTitulo do
   begin
     ANossoNumero := FormataNossoNumero(ACBrTitulo);
     wTamConvenio := Length(ACBrBanco.ACBrBoleto.Cedente.Convenio);
     wTamNossoNum := CalcularTamMaximoNossoNumero(ACBrTitulo.Carteira,
                                                  ACBrTitulo.NossoNumero);

     wCarteira:= StrToIntDef(Carteira, 0);
     if (((wCarteira = 11) or (wCarteira= 31) or (wCarteira = 51)) or
         ((wCarteira = 12) or (wCarteira = 15) or (wCarteira = 17))) and
        ((ACBrBoleto.Cedente.ResponEmissao <> tbCliEmite) and
         (StrToIntDef(NossoNumero,0) = 0)) then
     begin
       ANossoNumero := StringOfChar('0', 20);
       aDV          := ' ';
     end
     else
     begin
       ANossoNumero := FormataNossoNumero(ACBrTitulo);

       if (wTamConvenio = 7) or ((wTamConvenio = 6) and (wTamNossoNum = 17)) then
         aDV:= ''
       else
         aDV:= CalcularDigitoVerificador(ACBrTitulo);
     end;

     aAgencia := PadLeft(ACBrBoleto.Cedente.Agencia, 5, '0');
     aConta   := PadLeft(ACBrBoleto.Cedente.Conta, 12, '0');

     {SEGMENTO P}

     {Código para Protesto / Negativação}
      case CodigoNegativacao of
        cnProtestarCorrido :  ACodProtesto := '1';
        cnProtestarUteis   :  ACodProtesto := '2';
        cnNegativar        :  ACodProtesto := '8';
        cnNaoProtestar     :  ACodProtesto := '3';
      else
        case TipoDiasProtesto of
          diCorridos       : ACodProtesto := '1';
        else
          ACodProtesto     := '2';
        end;
      end;

      {Data e Dias de Protesto / Negativação}
      if (ACodProtesto = '8') then
      begin
        DataProtestoNegativacao := DateToStr(DataNegativacao);
        DiasProtestoNegativacao := IntToStr(DiasDeNegativacao);
      end
      else
      begin
        if (ACodProtesto <> '3') then
        begin
          DataProtestoNegativacao := DateToStr(DataProtesto);
          DiasProtestoNegativacao := IntToStr(DiasDeProtesto);
        end
        else
        begin
          DataProtestoNegativacao := '';
          DiasProtestoNegativacao := '0';
        end;
      end;

     {Pegando o Tipo de Ocorrencia}
     case OcorrenciaOriginal.Tipo of
       toRemessaBaixar                    : ATipoOcorrencia := '02';
       toRemessaConcederAbatimento        : ATipoOcorrencia := '04';
       toRemessaCancelarAbatimento        : ATipoOcorrencia := '05';
       toRemessaAlterarVencimento         : ATipoOcorrencia := '06';
       toRemessaConcederDesconto          : ATipoOcorrencia := '07';
       toRemessaCancelarDesconto          : ATipoOcorrencia := '08';
       toRemessaProtestar                 : ATipoOcorrencia := '09';
       toRemessaCancelarInstrucaoProtesto : ATipoOcorrencia := '10';
       toRemessaAlterarNomeEnderecoSacado : ATipoOcorrencia := '12';
       toRemessaDispensarJuros            : ATipoOcorrencia := '31';
     else
       ATipoOcorrencia := '01';
     end;

     { Pegando o tipo de EspecieDoc }
     if EspecieDoc = 'CH' then
       EspecieDoc   := '01'
     else if EspecieDoc = 'DM' then
       EspecieDoc   := '02'
     else if EspecieDoc = 'DMI' then
       EspecieDoc   := '03'
     else if EspecieDoc = 'DS' then
       EspecieDoc   := '04'
     else if EspecieDoc = 'DSI' then
       EspecieDoc   := '05'
     else if EspecieDoc = 'DR' then
       EspecieDoc   := '06'
     else if EspecieDoc = 'LC' then
       EspecieDoc   := '07'
     else if EspecieDoc = 'NCC' then
       EspecieDoc   := '08'
     else if EspecieDoc = 'NCE' then
       EspecieDoc   := '09'
     else if EspecieDoc = 'NCI' then
       EspecieDoc   := '10'
     else if EspecieDoc = 'NCR' then
       EspecieDoc   := '11'
     else if EspecieDoc = 'NP' then
       EspecieDoc   := '12'
     else if EspecieDoc = 'NPR' then
       EspecieDoc   := '13'
     else if EspecieDoc = 'TM' then
       EspecieDoc   := '14'
     else if EspecieDoc = 'TS' then
       EspecieDoc   := '15'
     else if EspecieDoc = 'NS' then
       EspecieDoc   := '16'
     else if EspecieDoc = 'RC' then
       EspecieDoc   := '17'
     else if EspecieDoc = 'FAT' then
       EspecieDoc   := '18'
     else if EspecieDoc = 'ND' then
       EspecieDoc   := '19'
     else if EspecieDoc = 'AP' then
       EspecieDoc   := '20'
     else if EspecieDoc = 'ME' then
       EspecieDoc   := '21'
     else if EspecieDoc = 'PC' then
       EspecieDoc   := '22';


     { Pegando o Aceite do Titulo }
     case Aceite of
       atSim :  ATipoAceite := 'A';
       atNao :  ATipoAceite := 'N';
     else
       ATipoAceite := 'N';
     end;

     BoletoEmail:= ACBrTitulo.CarteiraEnvio = tceBancoEmail;
     if BoletoEmail then
      begin
       QtdRegTitulo:= 4;
       GeraSegS    := True;
      end
     else
      begin
       QtdRegTitulo:= 3;
       GeraSegS    := False;
      end;     

     {Pegando Tipo de Boleto}
     case ACBrBoleto.Cedente.ResponEmissao of
       tbCliEmite : ATipoBoleto := '2' + '2';
       tbBancoEmite :
       begin
         if BoletoEmail then
           ATipoBoleto := '1' + '3'
         else
           ATipoBoleto := '1' + '1';
       end;
       tbBancoReemite :
       begin
         if BoletoEmail then
           ATipoBoleto := '4' + '3'
         else
           ATipoBoleto := '4' + '1';
       end;
       tbBancoNaoReemite : ATipoBoleto := '5' + '2';
     else
       ATipoBoleto := '2' + '2';
     end;

//     ACaracTitulo := ' ';
     case CaracTitulo of
       tcSimples       : ACaracTitulo  := '1';
       tcVinculada     : ACaracTitulo  := '2';
       tcCaucionada    : ACaracTitulo  := '3';
       tcDescontada    : ACaracTitulo  := '4';
       tcVendor        : ACaracTitulo  := '5';
       tcDiretaEspecial: ACaracTitulo  := '7';
     else
       ACaracTitulo  := '1';
     end;

     wCarteira:= StrToIntDef(Carteira,0);
     { 1    = Carteira 11/12 na modalidade Simples;
       2, 3 = Carteira 11/17 modalidade Vinculada/Caucionada e carteira 31;
       4    = Carteira 11/17 modalidade Descontada e carteira 51;
       7    = Carteira 17 modalidade Simples. }
     if ((wCarteira = 11) or (wCarteira = 12)) and (ACaracTitulo = '1') then
       wTipoCarteira := '1'
     else if (((wCarteira = 11) or (wCarteira = 17)) and
              ((ACaracTitulo = '2') or (ACaracTitulo = '3'))) or (wCarteira = 31) then
       wTipoCarteira := ACaracTitulo
     else if (((wCarteira = 11) or (wCarteira = 17)) and (ACaracTitulo = '4')) or
             (wCarteira = 51) then
       wTipoCarteira := ACaracTitulo
//     else  if (wCarteira = 17) and (ACaracTitulo = '1') then
//       wTipoCarteira := '7'
     else
       wTipoCarteira := '7';

     {Mora Juros}
     if (ValorMoraJuros > 0) and (DataMoraJuros > 0) then
       ADataMoraJuros := FormatDateTime('ddmmyyyy', DataMoraJuros)
     else
       ADataMoraJuros := PadRight('', 8, '0');

     {Código Mora}
     if CodigoMora = '' then
     begin
       if ValorMoraJuros > 0 then
         CodigoMora := '1'
       else
         CodigoMora := '3';
     end;

     {Descontos}
     if (ValorDesconto > 0) and (DataDesconto > 0) then
     begin
       if TipoDesconto = tdPercentualAteDataInformada then
         ACodigoDesconto := '2'
       else
         ACodigoDesconto := '1';
       ADataDesconto := FormatDateTime('ddmmyyyy', DataDesconto);
     end
     else
     begin
       if ValorDesconto > 0 then
         ACodigoDesconto := '3'
       else
         ACodigoDesconto := '0';
       ADataDesconto := PadRight('', 8, '0');
     end;

     if (ValorDesconto2 > 0) and (DataDesconto2 > DataDesconto) then
       ADataDesconto2 := FormatDateTime('ddmmyyyy', DataDesconto2)
     else
       ADataDesconto2 := PadRight('', 8, '0');

     if (ValorDesconto3 > 0) and (DataDesconto3 > DataDesconto2) then
       ADataDesconto3 := FormatDateTime('ddmmyyyy', DataDesconto3)
     else
       ADataDesconto3 := PadRight('', 8, '0');

     AMensagem   := '';
     if Mensagem.Text <> '' then
       AMensagem   := Mensagem.Strings[0];

     {Tipo Documento}
     ATipoDocumento:= DefineTipoDocumento;

     // Nº Dias para Baixa/Devolucao
     sDiasBaixa  := '   ';
     if ((ATipoOcorrencia = '01') or (ATipoOcorrencia = '39')) and (Max(DataBaixa, DataLimitePagto) > Vencimento) then
       sDiasBaixa  := IntToStrZero(DaysBetween(Vencimento, Max(DataBaixa, DataLimitePagto)), 3);

     {SEGMENTO P}
     Result:= IntToStrZero(ACBrBanco.Numero, 3)                                         + // 1 a 3 - Código do banco
              '0001'                                                                    + // 4 a 7 - Lote de serviço
              '3'                                                                       + // 8 - Tipo do registro: Registro detalhe
              IntToStrZero((QtdRegTitulo * ACBrBoleto.ListadeBoletos.IndexOf(ACBrTitulo)) + 1 , 5) + // 9 a 13 - Número seqüencial do registro no lote - Cada título tem 2 registros (P e Q)
              'P'                                                                       + // 14 - Código do segmento do registro detalhe
              ' '                                                                       + // 15 - Uso exclusivo FEBRABAN/CNAB: Branco
              ATipoOcorrencia                                                           + // 16 a 17 - Código de movimento
              aAgencia                                                                  + // 18 a 22 - Agência mantenedora da conta
              PadRight(ACBrBoleto.Cedente.AgenciaDigito, 1 , '0')                       + // 23 -Dígito verificador da agência
              aConta                                                                    + // 24 a 35 - Número da conta corrente
              PadRight(ACBrBoleto.Cedente.ContaDigito, 1, '0')                          + // 36 - Dígito verificador da conta
              ' '                                                                       + // 37 - Dígito verificador da agência / conta
              PadRight(ANossoNumero+aDV, 20, ' ')                                       + // 38 a 57 - Nosso número - identificação do título no banco
              wTipoCarteira                                                             + // 58 - Cobrança Simples
              '1'                                                                       + // 59 - Forma de cadastramento do título no banco: com cadastramento
              ATipoDocumento                                                            + // 60 - Tipo de documento: Tradicional
              ATipoBoleto                                                               + // 61 a 62 - Quem emite e quem distribui o boleto?
              PadRight(NumeroDocumento, 15, ' ')                                        + // 63 a 77 - Número que identifica o título na empresa [ Alterado conforme instruções da CSO Brasília ] {27-07-09}
              FormatDateTime('ddmmyyyy', Vencimento)                                    + // 78 a 85 - Data de vencimento do título
              IntToStrZero( round( ValorDocumento * 100), 15)                           + // 86 a 100 - Valor nominal do título
              '00000 '                                                                  + // 101 a 106 - Agência cobradora + Digito. Se ficar em branco, a caixa determina automaticamente pelo CEP do sacado
              PadRight(EspecieDoc,2)                                                    + // 107 a 108 - Espécie do documento
              ATipoAceite                                                               + // 109 - Identificação de título Aceito / Não aceito
              FormatDateTime('ddmmyyyy', DataDocumento)                                 + // 110 a 117 - Data da emissão do documento
              PadLeft(trim(CodigoMora), 1)                                              + // 118 - Código de mora (1=Valor diário; 2=Taxa Mensal; 3=Isento)
              ADataMoraJuros                                                            + // 119 a 126 - Data a partir da qual serão cobrados juros
              IfThen(ValorMoraJuros > 0,
                     IntToStrZero(round(ValorMoraJuros * 100), 15),
                     PadRight('', 15, '0'))                                             + // 127 a 141 - Valor de juros de mora por dia
              ACodigoDesconto                                                           + // 142 - Código de desconto: 1 - Valor fixo até a data informada, 2 - Percentual desconto 4-Desconto por dia de antecipacao 0 - Sem desconto
              ADataDesconto                                                             + // 143 a 150 - Data do desconto
              IfThen(ValorDesconto > 0, IntToStrZero( round(ValorDesconto * 100), 15),
                     PadRight('', 15, '0'))                                             + // 151 a 165 - Valor do desconto por dia
              IntToStrZero( round(ValorIOF * 100), 15)                                  + // 166 a 180 - Valor do IOF a ser recolhido
              IntToStrZero( round(ValorAbatimento * 100), 15)                           + // 181 a 195 - Valor do abatimento
              PadRight(SeuNumero, 25, ' ')                                              + // 196 a 220 - Identificação do título na empresa
//              IfThen((DataProtesto <> 0) and (DiasDeProtesto > 0), ACodProtesto, '3')   + // 221 - Código de protesto
//              IfThen((DataProtesto <> 0) and (DiasDeProtesto > 0),
//                    PadLeft(IntToStr(DiasDeProtesto), 2, '0'), '00')                    + // 222 a 223 - Prazo para protesto (em dias)
              IfThen((DataProtestoNegativacao <> '') and
                      (StrToInt(DiasProtestoNegativacao) > 0), ACodProtesto, '3')       + // 221 - Código de protesto
              IfThen((DataProtestoNegativacao <> '') and
                     (StrToInt(DiasProtestoNegativacao) > 0),
                      PadLeft(DiasProtestoNegativacao, 2, '0'), '00')                   + // 222 a 223 - Prazo para protesto (em dias)
              '0'                                                                       + // 224 - Campo não tratado pelo BB [ Alterado conforme instruções da CSO Brasília ] {27-07-09}
              sDiasBaixa                                                                + // 225 a 227 - Campo não tratado pelo BB [ Alterado conforme instruções da CSO Brasília ] {27-07-09}
              '09'                                                                      + // 228 a 229 - Código da moeda: Real
              StringOfChar('0', 10)                                                     + // 230 a 239 - Uso exclusivo FEBRABAN/CNAB
              ' ';
                                                                    // 240 - Uso exclusivo FEBRABAN/CNAB

     {SEGMENTO Q}
     Result:= Result + #13#10 +
              IntToStrZero(ACBrBanco.Numero, 3)                                        + // 1 - 3 Código do banco
              '0001'                                                                   + // 4 - 7 Número do lote
              '3'                                                                      + // 8 - 8 Tipo do registro: Registro detalhe
              IntToStrZero((QtdRegTitulo * ACBrBoleto.ListadeBoletos.IndexOf(ACBrTitulo)) + 2 ,5) + // 9 - 13 Número seqüencial do registro no lote - Cada título tem 2 registros (P e Q)
              'Q'                                                                      + // 14 - 14 Código do segmento do registro detalhe
              ' '                                                                      + // 15 - 15 Uso exclusivo FEBRABAN/CNAB: Branco
              ATipoOcorrencia                                                          + // 16 - 17 Tipo Ocorrencia
              IfThen(Sacado.Pessoa = pJuridica,'2','1')                                + // 18 - 18 Tipo inscricao
              PadLeft(OnlyNumber(Sacado.CNPJCPF), 15, '0')                             + // 19 - 33 Número da inscrição
              PadRight(Sacado.NomeSacado, 40, ' ')                                     + // 34 - 73 Nome
              PadRight(Sacado.Logradouro + ' ' + Sacado.Numero + ' '                   + // 74 - 113 Endereço
                       Sacado.Complemento , 40, ' ')                                   + // 114 - 128 Bairro
              PadRight(Sacado.Bairro, 15, ' ')                                         + // 129 - 133 CEP
              PadLeft(OnlyNumber(Sacado.CEP), 8, '0')                                  + // 134 - 136 Sufixo CEP
              PadRight(Sacado.Cidade, 15, ' ')                                         + // 137 - 151 Cidade
              PadRight(Sacado.UF, 2, ' ')                                              + // 152 - 153 UF
              IfThen(Sacado.SacadoAvalista.Pessoa = pJuridica,'2',
                     IfThen(Sacado.SacadoAvalista.CNPJCPF <> '','1', '0'))             + // 154 - 154 Tipo de inscrição: Não informado
              PadLeft(OnlyNumber(Sacado.SacadoAvalista.CNPJCPF), 15, '0')              + // 155 - 169 Número de inscrição
              PadRight(Sacado.SacadoAvalista.NomeAvalista, 40, ' ')                    + // 170 - 209 Nome do sacador/avalista
              PadRight('', 3, '0')                                                     + // 210 - 212 Uso exclusivo FEBRABAN/CNAB
              PadRight('',20, ' ')                                                     + // 213 - 232 Uso exclusivo FEBRABAN/CNAB
              PadRight('', 8, ' ');                                                      // 233 - 240 Uso exclusivo FEBRABAN/CNAB

     {SEGMENTO R}
     Result:= Result + #13#10 +
              IntToStrZero(ACBrBanco.Numero, 3)                                       + // 1 - 3 Código do banco
              '0001'                                                                  + // 4 - 7 Número do lote
              '3'                                                                     + // 8 - 8 Tipo do registro: Registro detalhe
              IntToStrZero((QtdRegTitulo * ACBrBoleto.ListadeBoletos.IndexOf(ACBrTitulo))+ 3 ,5) + // 9 - 13 Número seqüencial do registro no lote - Cada título tem 2 registros (P e Q)
              'R'                                                                     + // 14 - 14 Código do segmento do registro detalhe
              ' '                                                                     + // 15 - 15 Uso exclusivo FEBRABAN/CNAB: Branco
              ATipoOcorrencia                                                         + // 16 - 17 Tipo Ocorrencia

              ACodigoDesconto                                                         + // 18 - Código de desconto 2: Repetir o valor do código de desconto 1
              ADataDesconto2                                                          + // 19 a 26 - Data do desconto 2
              IfThen(ValorDesconto2 > 0, IntToStrZero( round(ValorDesconto2 * 100), 15),
                     PadRight('', 15, '0'))                                             + // 27 a 41 - Valor do desconto 2 por dia

              ACodigoDesconto                                                         +  // 42 - Código de desconto 3: Repetir o valor do código de desconto 1
              ADataDesconto3                                                          + // 43 a 50 - Data do desconto 3
              IfThen(ValorDesconto3 > 0, IntToStrZero( round(ValorDesconto3 * 100), 15),
                     PadRight('', 15, '0'))                                             + // 51 a 65 - Valor do desconto 3 por dia

              IfThen((PercentualMulta > 0),
                     IfThen(MultaValorFixo,'1','2'), '0')                             + // 66 - 66 1-Cobrar Multa Valor Fixo / 2-Percentual / 0-Não cobrar multa
              IfThen((PercentualMulta > 0),
                      FormatDateTime('ddmmyyyy', DataMulta), '00000000')              + // 67 - 74 Se cobrar informe a data para iniciar a cobrança ou informe zeros se não cobrar
              IfThen((PercentualMulta > 0),
                     IntToStrZero(round(PercentualMulta * 100), 15),PadRight('', 15, '0'))  + // 75 - 89 Valor / Percentual de multa. Informar zeros se não cobrar
              PadRight('',10,' ')                                                     + // 90 - 99  - Informação ao Sacado
              PadRight(AMensagem,40,' ')                                              + // 100 - 139  - Mensagem 3
              PadRight('',60,' ')                                                     + // 140 - 199  - Não tratado
              PadRight('',8,'0')                                                      + // 200 - 207
              StringOfChar('0', 24)                                                   + // 208 - 231 Zeros (De acordo com o manual de particularidades BB)
              StringOfChar(' ', 9);                                                     // 232 - 240 Brancos (De acordo com o manual de particularidades BB)

     {SEGMENTO S}
     if GeraSegS then
     begin
       Result := Result + #13#10 +
                IntToStrZero(ACBrBanco.Numero, 3)                                           + // 001 a 003 - Código do banco
                '0001'                                                                      + // 004 - 007 - Numero do lote remessa
                '3'                                                                         + // 008 - 008 - Tipo de registro
                IntToStrZero((QtdRegTitulo * ACBrBoleto.ListadeBoletos.IndexOf(ACBrTitulo))+ 4 ,5)     + // 009 - 013 - Número seqüencial do registro no lote - Cada título tem 2 registros (P e Q)
                'S'                                                                         + // 014 - 014 - Cód. Segmento do registro detalhe
                Space(1)                                                                    + // 015 - 015 - Reservado (uso Banco)
                ATipoOcorrencia;                                                              // 016 - 017 - Código de movimento remessa

       if BoletoEmail then
          Result:= Result + '8' + '00'                                                     + // 018 - 018 - Identificação da impressão // 019 - 020 - Zeros
                   PadRight(ACBrTitulo.Sacado.Email, 140)                                  + // 21-160  Email
                  '00'                                                                     + // 161-162 Zeros
                  Space(78)                                                                  // 163-240 brancos
       else
          Result:= Result + '3' + PadRight(MontarInstrucoes2, 222);

       inc(fQtMsg);
     end;

     {SEGMENTO S - FIM}
   end;
end;

function TACBrBancoBrasil.GerarRegistroTrailler240( ARemessa : TStringList ): String;
var
  wRegsLote: Integer;
begin
      wRegsLote := 3; // Total de Segmentos Obrigatórios

   {REGISTRO TRAILER DO LOTE}
   Result:= IntToStrZero(ACBrBanco.Numero, 3)                              + // 1 - 3 Código do banco
            '0001'                                                         + // 4 - 7 Número do lote
            '5'                                                            + // 8 - 8 Tipo do registro: Registro trailer do lote
            Space(9)                                                       + // 9 - 17 Uso exclusivo FEBRABAN/CNAB
            //IntToStrZero(ARemessa.Count-1, 6)                            + //Quantidade de Registro da Remessa
            IntToStrZero((wRegsLote * (ARemessa.Count-1)) + 2 + fQtMsg, 6) + // 18 - 23 Quantidade de Registro da Remessa
            PadRight('', 6, '0')                                           + //Quantidade títulos em cobrança
            PadRight('',17, '0')                                           + //Valor dos títulos em carteiras}
            PadRight('', 6, '0')                                           + //Quantidade títulos em cobrança
            PadRight('',17, '0')                                           + //Valor dos títulos em carteiras}
            PadRight('', 6, '0')                                           + //Quantidade títulos em cobrança
            PadRight('',17, '0')                                           + //Valor dos títulos em carteiras}
            PadRight('', 6, '0')                                           + //Quantidade títulos em cobrança
            PadRight('',17, '0')                                           + //Valor dos títulos em carteiras}
            Space(8)                                                       + //Uso exclusivo FEBRABAN/CNAB}
            PadRight('',117,' ')                                           ;

   {GERAR REGISTRO TRAILER DO ARQUIVO}
   Result:= Result + #13#10 +
            IntToStrZero(ACBrBanco.Numero, 3)                              + //Código do banco
            '9999'                                                         + //Lote de serviço
            '9'                                                            + //Tipo do registro: Registro trailer do arquivo
            space(9)                                                       + //Uso exclusivo FEBRABAN/CNAB}
            '000001'                                                       + //Quantidade de lotes do arquivo}
            IntToStrZero(((ARemessa.Count-1)* wRegsLote)+ 4 + fQtMsg, 6)   + //Quantidade de registros do arquivo, inclusive este registro que está sendo criado agora}
            space(6)                                                       + //Uso exclusivo FEBRABAN/CNAB}
            space(205);                                                      //Uso exclusivo FEBRABAN/CNAB}

   fQtMsg := 0;
end;


procedure TACBrBancoBrasil.GerarRegistroHeader400(NumeroRemessa: Integer; aRemessa:TStringList);
var
  TamConvenioMaior6 :Boolean;
  aAgencia, aConta  :String;
  wLinha: String;
begin
  with ACBrBanco.ACBrBoleto.Cedente do
  begin
    TamConvenioMaior6:= Length(trim(Convenio)) > 6;
    aAgencia:= RightStr(Agencia, 4);
    aConta  := RightStr(Conta, 8);

    wLinha:= '0'                              + // ID do Registro
             '1'                              + // ID do Arquivo( 1 - Remessa)
             'REMESSA'                        + // Literal de Remessa
             '01'                             + // Código do Tipo de Serviço
             PadRight( 'COBRANCA', 15 )       + // Descrição do tipo de serviço
             aAgencia                         + // Prefixo da agência/ onde esta cadastrado o convenente lider do cedente
             PadRight( AgenciaDigito, 1, ' ') + // DV-prefixo da agencia
             aConta                           + // Codigo do cedente/nr. da conta corrente que está cadastro o convenio lider do cedente
             PadRight( ContaDigito, 1, ' ');    // DV-código do cedente


    if TamConvenioMaior6 then
      wLinha:= wLinha + '000000'                       // Complemento
    else
      wLinha:= wLinha + PadLeft(trim(Convenio),6,'0'); // Convenio;

    wLinha:= wLinha + PadRight( Nome, 30)     + // Nome da Empresa
             IntToStrZero( Numero, 3)         + // Código do Banco
             PadRight(UpperCase(fpNome), 15)  + // Nome do Banco(BANCO DO BRASIL)
             FormatDateTime('ddmmyy',Now)     + // Data de geração do arquivo
             IntToStrZero(NumeroRemessa,7);     // Numero Remessa

    if TamConvenioMaior6 then
      wLinha:= wLinha + Space(22)                                        + // Nr. Sequencial de Remessa + brancos
               PadLeft(trim(ACBrBanco.ACBrBoleto.Cedente.Convenio),7,'0')+ // Nr. Convenio
               space(258)                                                  // Brancos
    else
      wLinha:= wLinha + Space(287);

      wLinha:= wLinha + IntToStrZero(1,6); // Nr. Sequencial do registro-informar 000001

      aRemessa.Text:= aRemessa.Text + UpperCase(wLinha);
  end;
end;

procedure TACBrBancoBrasil.GerarRegistroTransacao400(ACBrTitulo: TACBrTitulo; aRemessa: TStringList);
var
  ANossoNumero, ADigitoNossoNumero :String;
  ATipoOcorrencia, AInstrucao      :String;
  ATipoSacado, ATipoCendente       :String;
  ATipoAceite, ATipoEspecieDoc     :String;
  AMensagem, DiasProtesto          :String;
  aDataDesconto, aAgencia, aConta  :String;
  aModalidade,wLinha, aTipoCobranca:String;
  TamConvenioMaior6                :Boolean;
  wCarteira, LDiasProtesto, LDiasTrabalhados : Integer;
  sDiasBaixa: String;
begin

   with ACBrTitulo do
   begin
     wCarteira:= StrToIntDef(Carteira,0);
     if ((wCarteira = 11) or (wCarteira= 31) or (wCarteira = 51)) or
        (((wCarteira = 12) or (wCarteira = 15) or (wCarteira = 17)) and
         (ACBrBoleto.Cedente.ResponEmissao <> tbCliEmite)) then
      begin
       ANossoNumero       := '00000000000000000000';
       ADigitoNossoNumero := ' ';
      end
     else
      begin
       ANossoNumero       := FormataNossoNumero(ACBrTitulo);
       ADigitoNossoNumero := CalcularDigitoVerificador(ACBrTitulo);
      end;
      
     TamConvenioMaior6:= Length(trim(ACBrBoleto.Cedente.Convenio)) > 6;
     aAgencia         := PadLeft(ACBrBoleto.Cedente.Agencia, 4, '0');
     aConta           := RightStr(ACBrBoleto.Cedente.Conta, 8);
     aModalidade      := PadLeft(trim(ACBrBoleto.Cedente.Modalidade), 3, '0');

     {Pegando Código da Ocorrencia}
     case OcorrenciaOriginal.Tipo of
       toRemessaBaixar                         : ATipoOcorrencia := '02'; {Pedido de Baixa}
       toRemessaConcederAbatimento             : ATipoOcorrencia := '04'; {Concessão de Abatimento}
       toRemessaCancelarAbatimento             : ATipoOcorrencia := '05'; {Cancelamento de Abatimento concedido}
       toRemessaAlterarVencimento              : ATipoOcorrencia := '06'; {Alteração de vencimento}
       toRemessaAlterarControleParticipante    : ATipoOcorrencia := '07'; {Alteração do número de controle do participante}
       toRemessaAlterarNumeroControle          : ATipoOcorrencia := '08'; {Alteração de seu número}
       toRemessaProtestar                      : ATipoOcorrencia := '09'; {Pedido de protesto}
       toRemessaCancelarInstrucaoProtestoBaixa : ATipoOcorrencia := '10'; {Sustar protesto e baixar}
       toRemessaCancelarInstrucaoProtesto      : ATipoOcorrencia := '10'; {Sustar protesto e manter na carteira}
       toRemessaDispensarJuros                 : ATipoOcorrencia := '11'; {Instrução para dispensar juros}
       toRemessaAlterarNomeEnderecoSacado      : ATipoOcorrencia := '12'; {Alteração de nome e endereço do Sacado}
       toRemessaConcederDesconto               : ATipoOcorrencia := '31'; {Conceder desconto}
       toRemessaCancelarDesconto               : ATipoOcorrencia := '32'; {Não conceder desconto}
       toRemessaDispensarMulta                 : ATipoOcorrencia := '36';
       toRemessaDispensarPrazoLimiteRecebimento: ATipoOcorrencia := '38';
       toRemessaAlterarPrazoLimiteRecebimento  : ATipoOcorrencia := '39';
       toRemessaAlterarModalidade              : ATipoOcorrencia := '40'; {Alterar modalidade (Vide Observações)}
     else
       ATipoOcorrencia := '01'; {Remessa}
     end;

     { Pegando o Aceite do Titulo }
     case Aceite of
       atSim :  ATipoAceite := 'A';
       atNao :  ATipoAceite := 'N';
     else
       ATipoAceite := 'N';
     end;

     { Pegando o tipo de EspecieDoc }
     if EspecieDoc = 'DM' then
       ATipoEspecieDoc   := '01'
     else if EspecieDoc = 'NP' then
       ATipoEspecieDoc   := '02'
     else if EspecieDoc = 'NS' then
       ATipoEspecieDoc   := '03'
     else if EspecieDoc = 'RC' then
       ATipoEspecieDoc   := '05'
     else if EspecieDoc = 'LC' then
       ATipoEspecieDoc   := '08'
     else if EspecieDoc = 'DS' then
       ATipoEspecieDoc   := '12'
     else if EspecieDoc = 'ND' then
       ATipoEspecieDoc   := '13';

     { Pegando Tipo de Cobrança}
     case StrToInt(ACBrTitulo.Carteira) of
       11,17 :
          case ACBrBoleto.Cedente.CaracTitulo of
            tcSimples   : aTipoCobranca := '     ';
            tcDescontada: aTipoCobranca := '04DSC';
            tcVendor    : aTipoCobranca := '08VDR';
            tcVinculada : aTipoCobranca := '02VIN';
          else
            aTipoCobranca := '     ';
          end;
     else
       aTipoCobranca:='     ';
     end;

     Instrucao1 := trim(IfThen(Instrucao1='00', '', Instrucao1));
     Instrucao2 := trim(IfThen(Instrucao2='00', '', Instrucao2));

     if (((Instrucao1 <> '') or (Instrucao2 <> '')) and (Instrucao1 = Instrucao2 )) then
           raise Exception.Create(ACBrStr('A Instrução1 não pode ser igual a Instrução2.'));

     //Verifica se foi informado data instrucao de protesto e data de protesto

     if( ( Instrucao1='88' ) or (Instrucao2='88')) then
     begin
       Instrucao1 := IfThen(Instrucao1='06', '00', Instrucao1);
       Instrucao2 := IfThen(Instrucao2='06', '00', Instrucao2);
       DiasProtesto := IntToStr( DiasDeProtesto );
       AInstrucao  := PadLeft(Instrucao1,2,'0') + PadLeft(Instrucao2,2,'0');
     end
     else
     begin
       AInstrucao := PadLeft(Instrucao1,2,'0') + PadLeft(Instrucao2,2,'0');

       if (DataProtesto > 0) and (DataProtesto > Vencimento) then
       begin
         DiasProtesto := '  ';
         LDiasProtesto := DaysBetween(DataProtesto,Vencimento);
         if TipoDiasProtesto = diCorridos then
            LDiasTrabalhados := LDiasProtesto
         else
            LDiasTrabalhados := WorkingDaysBetween(ACBrTitulo.Vencimento,ACBrTitulo.DataProtesto) ;

         if ((LDiasTrabalhados in [03,04,05])  or
             (LDiasTrabalhados in [03,04,05]))  then
          begin
           if ((Instrucao1 = '') or (Instrucao1 = PadLeft(inttostr(LDiasTrabalhados),2,'0')))  then
              AInstrucao  := PadLeft(inttostr(LDiasTrabalhados),2,'0') + IfThen(Instrucao2 = PadLeft(inttostr(LDiasTrabalhados),2,'0'), '00',PadLeft(Instrucao2,2,'0'))
           else if ((Instrucao2 = '') or (Instrucao2 = PadLeft(inttostr(LDiasTrabalhados),2,'0'))) then
              AInstrucao  := IfThen(Instrucao1 = PadLeft(inttostr(LDiasTrabalhados),2,'0'), '00',PadLeft(Instrucao1,2,'0')) +PadLeft(inttostr(LDiasTrabalhados),2,'0');
          end
         else
          begin
           if ((Instrucao1 = '') or (Instrucao1 = '06')) then
              AInstrucao  := '06' + IfThen(Instrucao2 = '06', '00',PadLeft(Instrucao2,2,'0'))
           else if ((Instrucao2 = '') or (Instrucao2 = '06')) then
              AInstrucao  := IfThen(Instrucao1 = '06', '00',PadLeft(Instrucao1,2,'0')) +'06';
           DiasProtesto :=IntToStr(DaysBetween(DataProtesto,Vencimento));
          end;

         if ( (ACBrTitulo.TipoDiasProtesto = diCorridos) and (LDiasProtesto >= 6)
           and ( ((Instrucao1 = '') or (Instrucao1 = '06'))
              or ((Instrucao2 = '') or (Instrucao2 = '06')) )) then
         begin

           if ((Instrucao1 = '') or (Instrucao1 = '06')) then
              AInstrucao   := '06'+ PadLeft(Instrucao2,2,'0');

           if ((Instrucao2 = '') or (Instrucao2 = '06')) and (Instrucao1 <> '06') then
              AInstrucao   := PadLeft(Instrucao1,2,'0')+'06';

           DiasProtesto := IntToStr(LDiasProtesto);
         end;

        end
       else if ATipoOcorrencia <> '02' then //para comando de baixa 02 é necessario informar a instrução [42,44 ou 46]
       begin
         {
         Comando 07 de nao protestar, somente em comando, devido os tipos de carteiras
         if (((Instrucao1 = '') or (Instrucao1 = '07')) and (Instrucao2 <> '07')) then
           AInstrucao  := '07' + IfThen(Instrucao2 = '07', '00',PadLeft(Instrucao2,2,'0'))
         else if ((Instrucao2 = '') or (Instrucao2 = '07')) then
           AInstrucao  := IfThen(Instrucao1 = '07', '00',PadLeft(Instrucao1,2,'0')) +'07';
         }
         DiasProtesto:= '  ';
       end;

       //Verificando se existir comandos 09 - 03,04,05,10,15,20,25,30,45 na Instrucao1 ou Instrucao2-> qtde dias 392 = ' '
       if ((StrToIntDef(Instrucao1,0) in [03,04,05,10,15,20,25,30,35,40,45])  or
           (StrToIntDef(Instrucao2,0) in [03,04,05,10,15,20,25,30,35,40,45]))  then
       begin
        Instrucao1  := IfThen(Instrucao1='06', '00', Instrucao1);
        Instrucao2  := IfThen(Instrucao2='06', '00', Instrucao2);
        DiasProtesto:= '  ';
        AInstrucao  := PadLeft(Instrucao1,2,'0') + PadLeft(Instrucao2,2,'0');
       end;


     end;

      aDataDesconto:= '000000';

     if ValorDesconto > 0 then
     begin
       if DataDesconto > EncodeDate(2000,01,01) then
         aDataDesconto := FormatDateTime('ddmmyy',DataDesconto)
       else
         aDataDesconto := '777777';
     end;

     {Pegando Tipo de Sacado}
     case Sacado.Pessoa of
       pFisica   : ATipoSacado := '01';
       pJuridica : ATipoSacado := '02';
     else
       ATipoSacado := '00';
     end;

     {Pegando Tipo de Cedente}
     case ACBrBoleto.Cedente.TipoInscricao of
       pFisica   : ATipoCendente := '01';
       pJuridica : ATipoCendente := '02';
     else
       ATipoCendente := '02';
     end;

     AMensagem   := '';
     if (Sacado.SacadoAvalista.CNPJCPF <> '') then
     begin
       if Sacado.SacadoAvalista.Pessoa = pJuridica then
         AMensagem := Copy(Sacado.SacadoAvalista.NomeAvalista, 1, 21) + ' CNPJ' + Sacado.SacadoAvalista.CNPJCPF
       else
         AMensagem := Copy(Sacado.SacadoAvalista.NomeAvalista, 1, 25) + ' CPF' + Sacado.SacadoAvalista.CNPJCPF;
     end;

     if Mensagem.Text <> '' then
       AMensagem   := Mensagem.Strings[0];

     sDiasBaixa := '   ';
     if ((ATipoOcorrencia = '01') or (ATipoOcorrencia = '39')) and (Max(DataBaixa, DataLimitePagto) > Vencimento ) then
       sDiasBaixa := IntToStrZero(DaysBetween(Vencimento, Max(DataBaixa, DataLimitePagto)), 3);
     if ATipoOcorrencia = '39' then
     begin
       Instrucao1:= '00';
       Instrucao2:= '00';
       AInstrucao:= '0000';
       aDataDesconto:= sDiasBaixa + '000';
     end;

     with ACBrBoleto do
     begin
       if TamConvenioMaior6 then
         wLinha := '7'
       else
         wLinha := '1';

       wLinha:= wLinha                                     + // ID Registro
                ATipoCendente                              + // Tipo de inscrição da empresa
                PadLeft(OnlyNumber(Cedente.CNPJCPF),14,'0')+ // Inscrição da empresa
                aAgencia                                   + // Prefixo da agencia
                PadRight( Cedente.AgenciaDigito, 1)        + // DV-prefixo da agencia
                aConta                                     + // Código do cendete/nr. conta corrente da empresa
                PadRight( Cedente.ContaDigito, 1);           // DV-código do cedente

       if TamConvenioMaior6 then
         wLinha:= wLinha + PadLeft( trim(Cedente.Convenio), 7)  // Número do convenio
       else
         wLinha:= wLinha + PadLeft( trim(Cedente.Convenio), 6); // Número do convenio

       wLinha:= wLinha + PadRight( SeuNumero, 25 );             // Numero de Controle do Participante

       if TamConvenioMaior6 then
         wLinha:= wLinha + PadLeft( ANossoNumero, 17, '0')      // Nosso numero
       else
         wLinha:= wLinha + PadLeft( ANossoNumero,11)+ ADigitoNossoNumero;


       wLinha:= wLinha + '0000' + Space(3);                // Zeros + Brancos + Prefixo do titulo + Variação da carteira

       if (Sacado.SacadoAvalista.CNPJCPF <> '') then//Indica se Mensagem ou Avalista
         wLinha := wLinha + 'A'//Avalista
       else
         wLinha := wLinha + Space(1);//Mensagem

       wLinha:= wLinha + Space(3) + aModalidade;                // Zeros + Brancos + Prefixo do titulo + Variação da carteira

       if TamConvenioMaior6  then
         wLinha:= wLinha + IntToStrZero(0,7)                    // Zero + Zeros + Zero + Zeros
       else
         wLinha:= wLinha + IntToStrZero(0,13);

       wLinha:= wLinha                                                  +
                aTipoCobranca                                           + // Tipo de cobrança - 11, 17 (04DSC, 08VDR, 02VIN, BRANCOS) 12,31,51 (BRANCOS)
                Carteira                                                + // Carteira
                ATipoOcorrencia                                         + // Ocorrência "Comando"
                PadRight( NumeroDocumento, 10, ' ')                     + // Seu Numero - Nr. titulo dado pelo cedente
                FormatDateTime( 'ddmmyy', Vencimento )                  + // Data de vencimento
                IntToStrZero( Round( ValorDocumento * 100 ), 13)        + // Valor do titulo
                '001' + '0000' + ' '                                    + // Numero do Banco - 001 + Prefixo da agencia cobradora + DV-pref. agencia cobradora
                PadLeft(ATipoEspecieDoc, 2, '0') + ATipoAceite          + // Especie de titulo + Aceite
                FormatDateTime( 'ddmmyy', DataDocumento )               + // Data de Emissão
                AInstrucao                                              + // 1ª e 2ª instrução codificada
                IntToStrZero( round(ValorMoraJuros * 100 ), 13)         + // Juros de mora por dia
                aDataDesconto                                           + // Data limite para concessao de desconto
                IntToStrZero( round( ValorDesconto * 100), 13)          + // Valor do desconto
                IntToStrZero( round( ValorIOF * 100 ), 13)              + // Valor do IOF
                IntToStrZero( round( ValorAbatimento * 100 ), 13)       + // Valor do abatimento permitido
                ATipoSacado + PadLeft(OnlyNumber(Sacado.CNPJCPF),14,'0')+ // Tipo de inscricao do sacado + CNPJ ou CPF do sacado
                PadRight( Sacado.NomeSacado, 37) + '   '                + // Nome do sacado + Brancos
                PadRight(trim(Sacado.Logradouro) + ', ' +
                         trim(Sacado.Numero) + ', '+
                         trim(Sacado.Complemento), 40)                  + // Endereço do sacado
                PadRight( Trim(Sacado.Bairro), 12)                      +
                PadLeft( OnlyNumber(Sacado.CEP), 8 )                    + // CEP do endereço do sacado
                PadRight( trim(Sacado.Cidade), 15)                      + // Cidade do sacado
                PadRight( Sacado.UF, 2 )                                + // UF da cidade do sacado
                PadRight( AMensagem, 40)                                + // Observações

                IfThen(DiasDeNegativacao > 0,
                  PadLeft(IntToStr(DiasDeNegativacao),2,'0'),
                  PadLeft(DiasProtesto,2,'0')  ) + ' '                  + // Número de dias para protesto ou negativacao + Branco

                IntToStrZero( aRemessa.Count + 1, 6 );

       if ATipoOcorrencia = '01' then
       begin
         wLinha:= wLinha + sLineBreak                            +
                '5'                                              + //Tipo Registro
                '99'                                             + //Tipo de Serviço (Cobrança de Multa)
                IfThen((PercentualMulta > 0),
                       IfThen(MultaValorFixo,'1','2'), '9')      + //Cod. 1-Cobrar Multa Valor Fixo / 2-Percentual / 9-Não cobrar multa
                IfThen(PercentualMulta > 0,
                       FormatDateTime('ddmmyy', DataMulta),
                                      '000000')                  + //Data Multa
                IntToStrZero( round( PercentualMulta * 100), 12) + //Perc. Multa
                sDiasBaixa                                       + //Qtd dias Recebimento após vencimento
                Space(369)                                       + //Brancos
                IntToStrZero(aRemessa.Count + 2 ,6);
       end;

       aRemessa.Text := aRemessa.Text + UpperCase(wLinha);
     end;
   end;
end;

procedure TACBrBancoBrasil.GerarRegistroTrailler400(
  ARemessa: TStringList);
var
  wLinha: String;
begin
   wLinha := '9' + Space(393)                     + // ID Registro
             IntToStrZero(ARemessa.Count + 1, 6);   // Contador de Registros

   ARemessa.Text:= ARemessa.Text + UpperCase(wLinha);
end;

procedure TACBrBancoBrasil.LerRetorno240(ARetorno: TStringList);
var
  Titulo: TACBrTitulo;
  TempData, Linha, rCedente, rCNPJCPF: String;
  ContLinha : Integer;
  idxMotivo: Integer;
  rConvenioCedente: String;
  CodMotivo: Integer;
begin
   // informação do Header
   // Verifica se o arquivo pertence ao banco
   if StrToIntDef(copy(ARetorno.Strings[0], 1, 3),-1) <> Numero then
      raise Exception.create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                             'não' + 'é um arquivo de retorno do ' + Nome));

   ACBrBanco.ACBrBoleto.DataArquivo := StringToDateTimeDef(Copy(ARetorno[0],144,2)+'/'+
                                                           Copy(ARetorno[0],146,2)+'/'+
                                                           Copy(ARetorno[0],148,4),0, 'DD/MM/YYYY' );

   ACBrBanco.ACBrBoleto.NumeroArquivo := StrToIntDef(Copy(ARetorno[0],158,6),0);

   rCedente        := trim(copy(ARetorno[0], 73, 30));
   if copy(ARetorno[0], 18, 1) = '1' then
      rCNPJCPF        := OnlyNumber((copy(ARetorno[0], 22, 11)))
   else
      rCNPJCPF        := OnlyNumber((copy(ARetorno[0], 19, 14)));
   rConvenioCedente:= Trim(RemoveZerosEsquerda(Copy(ARetorno[0], 33, 9)));

   ValidarDadosRetorno('', '', rCNPJCPF);
   with ACBrBanco.ACBrBoleto do
   begin
     if LeCedenteRetorno then
     begin
       case StrToIntDef(copy(ARetorno[0], 18, 1), 0) of
         01:
           Cedente.TipoInscricao := pFisica;
         else
           Cedente.TipoInscricao := pJuridica;
       end;

       Cedente.Nome          := rCedente;
       Cedente.CNPJCPF       := rCNPJCPF;
       Cedente.Convenio      := rConvenioCedente;
       Cedente.Agencia       := trim(copy(ARetorno[0], 53, 5));
       Cedente.AgenciaDigito := trim(copy(ARetorno[0], 58, 1));
       Cedente.Conta         := trim(copy(ARetorno[0], 59, 12));
       Cedente.ContaDigito   := trim(copy(ARetorno[0], 71, 1));
     end;

     ACBrBanco.ACBrBoleto.ListadeBoletos.Clear;
   end;

   ACBrBanco.TamanhoMaximoNossoNum := 20;  
   Linha := '';
   Titulo := nil;

   for ContLinha := 1 to ARetorno.Count - 2 do
   begin
      Linha := ARetorno[ContLinha];

      if copy(Linha, 8, 1) <> '3' then // verifica se o registro (linha) é um registro detalhe (segmento J)
         Continue;

      if copy(Linha, 14, 1) = 'T' then // se for segmento T cria um novo titulo
         Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

      if Assigned(Titulo) then
      with Titulo do
      begin
         if copy(Linha, 14, 1) = 'T' then
          begin
            SeuNumero := copy(Linha, 106, 25);
            NumeroDocumento := copy(Linha, 59, 15);
            Carteira := copy(Linha, 58, 1);

            TempData := copy(Linha, 74, 2) + '/'+copy(Linha, 76, 2)+'/'+copy(Linha, 78, 4);
            if TempData<>'00/00/0000' then
              Vencimento := StringToDateTimeDef(TempData, 0, 'DD/MM/YYYY');

            ValorDocumento := StrToFloatDef(copy(Linha, 82, 15), 0) / 100;

            NossoNumero := NossoNumeroSemFormatacaoLerRetorno(ACBrBoleto.Cedente.Convenio, Carteira, Linha);

            ValorDespesaCobranca := StrToFloatDef(copy(Linha, 199, 15), 0) / 100;

            OcorrenciaOriginal.Tipo := CodOcorrenciaToTipo(StrToIntDef(copy(Linha, 16, 2), 0));

            IdxMotivo := 214;

            while (IdxMotivo < 223) do
            begin
               if (trim(Copy(Linha, IdxMotivo, 2)) <> '') then
               begin
                  MotivoRejeicaoComando.Add(Copy(Linha, IdxMotivo, 2));

                  case AnsiIndexStr(Copy(Linha, IdxMotivo, 2), ['A1','A2','A3','A4','A5','A6','A7','A8','A9']) of
                    0: CodMotivo := 101;
                    1: CodMotivo := 102;
                    2: CodMotivo := 103;
                    3: CodMotivo := 104;
                    4: CodMotivo := 105;
                    5: CodMotivo := 106;
                    6: CodMotivo := 107;
                    7: CodMotivo := 108;
                    8: CodMotivo := 109;
                  else
                    CodMotivo := StrToIntDef(Copy(Linha, IdxMotivo, 2),0);
                  end;
                  DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo, CodMotivo));
               end;
               Inc(IdxMotivo, 2);
            end;

          end
         else if copy(Linha, 14, 1) = 'U' then // segmento U
          begin
            CodigoLiquidacao    := copy(Linha, 16, 2);  //Código de Movimento Retorno  página 156
            ValorIOF            := StrToFloatDef(copy(Linha, 63, 15), 0) / 100;
            ValorAbatimento     := StrToFloatDef(copy(Linha, 48, 15), 0) / 100;
            ValorDesconto       := StrToFloatDef(copy(Linha, 33, 15), 0) / 100;
            ValorMoraJuros      := StrToFloatDef(copy(Linha, 18, 15), 0) / 100;
            ValorOutrosCreditos := StrToFloatDef(copy(Linha, 123, 15), 0) / 100;
            ValorOutrasDespesas := StrToFloatDef(copy(Linha, 108, 15), 0) / 100;
            ValorPago           := StrToFloatDef(copy(Linha, 78, 15), 0) / 100;
            ValorRecebido       := StrToFloatDef(copy(Linha, 93, 15), 0) / 100;
            TempData := copy(Linha, 138, 2)+'/'+copy(Linha, 140, 2)+'/'+copy(Linha, 142, 4);
            if TempData<>'00/00/0000' then
              DataOcorrencia := StringToDateTimeDef(TempData, 0, 'DD/MM/YYYY');
            TempData := copy(Linha, 146, 2)+'/'+copy(Linha, 148, 2)+'/'+copy(Linha, 150, 4);
            if TempData<>'00/00/0000' then
              DataCredito := StringToDateTimeDef(TempData, 0, 'DD/MM/YYYY');
          end;
      end;
   end;

   ACBrBanco.TamanhoMaximoNossoNum := 10;
end;

function TACBrBancoBrasil.TipoOcorrenciaToCod (
   const TipoOcorrencia: TACBrTipoOcorrencia ) : String;
begin
   Result := '';

   if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
   begin
     case TipoOcorrencia of
       toRetornoRegistroRecusado                    : Result := '03';
       toRetornoTransferenciaCarteiraEntrada        : Result := '04';
       toRetornoTransferenciaCarteiraBaixa          : Result := '05';
       toRetornoBaixaAutomatica                     : Result := '09';
       toRetornoBaixadoFrancoPagamento              : Result := '15';
       toRetornoLiquidadoSemRegistro                : Result := '17';
       toRetornoRecebimentoInstrucaoSustarProtesto  : Result := '20';
       toRetornoRetiradoDeCartorio                  : Result := '24';
       toRetornoBaixaPorProtesto                    : Result := '25';
       toRetornoInstrucaoRejeitada                  : Result := '26';
       toRetornoAlteracaoUsoCedente                 : Result := '27';
       toRetornoDebitoTarifas                       : Result := '28';
       toRetornoOcorrenciasDoSacado                 : Result := '29';
       toRetornoAlteracaoDadosRejeitados            : Result := '30';
       toRetornoChequePendenteCompensacao           : Result := '50';
       toRetornoInclusaoNegativacao                 : Result := '85';
       toRetornoExclusaoNegativacao                 : Result := '86';
     end;
   end
   else
   begin
     case TipoOcorrencia of
       toRetornoComandoRecusado                    : Result := '03';
       toRetornoLiquidadoSemRegistro               : Result := '05';
       toRetornoLiquidadoPorConta                  : Result := '07';
       toRetornoLiquidadoSaldoRestante             : Result := '08';
       toRetornoBaixaSolicitada                    : Result := '10';
       toRetornoLiquidadoEmCartorio                : Result := '15';
       toRetornoConfirmacaoAlteracaoJurosMora      : Result := '16';
       toRetornoDebitoEmConta                      : Result := '20';
       toRetornoNomeSacadoAlterado                 : Result := '21';
       toRetornoEnderecoSacadoAlterado             : Result := '22';
	   toRetornoEncaminhadoACartorio               : Result := '23';
       toRetornoProtestoSustado                    : Result := '24';
       toRetornoJurosDispensados                   : Result := '25';
       toRetornoManutencaoTituloVencido            : Result := '28';
       toRetornoDescontoConcedido                  : Result := '31';
       toRetornoDescontoCancelado                  : Result := '32';
       toRetornoDescontoRetificado                 : Result := '33';
       toRetornoAlterarDataDesconto                : Result := '34';
       toRetornoRecebimentoInstrucaoAlterarJuros   : Result := '35';
       toRetornoRecebimentoInstrucaoDispensarJuros : Result := '36';
       toRetornoDispensarIndexador                 : Result := '37';
       toRetornoDispensarPrazoLimiteRecebimento    : Result := '38';
       toRetornoAlterarPrazoLimiteRecebimento      : Result := '39';
       toRetornoAcertoControleParticipante         : Result := '41';
       toRetornoTituloPagoEmCheque                 : Result := '46';
       toRetornoTipoCobrancaAlterado               : Result := '72';
       toRetornoInclusaoNegativacao                : Result := '85';
       toRetornoExclusaoNegativacao                : Result := '86';
       toRetornoDespesasProtesto                   : Result := '96';
       toRetornoDespesasSustacaoProtesto           : Result := '97';
       toRetornoDebitoCustasAntecipadas            : Result := '98';
     end;
   end;

   if (Result <> '') then
   Exit;

   case TipoOcorrencia of
     toRetornoRegistroConfirmado                   : Result := '02';
     toRetornoLiquidado                            : Result := '06';
     toRetornoBaixaAutomatica                      : Result := '09';
     toRetornoTituloEmSer                          : Result := '11';
     toRetornoAbatimentoConcedido                  : Result := '12';
     toRetornoAbatimentoCancelado                  : Result := '13';
     toRetornoVencimentoAlterado                   : Result := '14';
     toRetornoRecebimentoInstrucaoProtestar        : Result := '19';
     toRetornoEntradaEmCartorio                    : Result := '23';
     toRetornoChequeDevolvido                      : Result := '44';
   else
     Result := '02';
   end;
end;

function TACBrBancoBrasil.TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String;
var
 CodOcorrencia: Integer;
begin

  Result := '';
  CodOcorrencia := StrToIntDef(TipoOcorrenciaToCod(TipoOcorrencia),0);

  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
  begin
    case CodOcorrencia of
      02: Result:= '02 – Entrada confirmada';
      03: Result:= '03 – Entrada Rejeitada';
      04: Result:= '04 – Transferência de Carteira/Entrada';
      05: Result:= '05 – Transferência de Carteira/Baixa';
      06: Result:= '06 – Liquidação';
      09: Result:= '09 – Baixa';
      11: Result:= '11 – Títulos em Carteira (em ser)';
      12: Result:= '12 – Confirmação Recebimento Instrução de Abatimento';
      13: Result:= '13 – Confirmação Recebimento Instrução de Cancelamento Abatimento';
      14: Result:= '14 – Confirmação Recebimento Instrução Alteração de Vencimento';
      15: Result:= '15 – Franco de Pagamento';
      17: Result:= '17 – Liquidação Após Baixa ou Liquidação Título Não Registrado';
      19: Result:= '19 – Confirmação Recebimento Instrução de Protesto';
      20: Result:= '20 – Confirmação Recebimento Instrução de Sustação/Cancelamento de Protesto';
      23: Result:= '23 – Remessa a Cartório';
      24: Result:= '24 – Retirada de Cartório e Manutenção em Carteira';
      25: Result:= '25 – Protestado e Baixado';
      26: Result:= '26 – Instrução Rejeitada';
      27: Result:= '27 – Confirmação do Pedido de Alteração de Outros Dados';
      28: Result:= '28 – Débito de Tarifas/Custas';
      29: Result:= '29 – Ocorrências do Sacado';
      30: Result:= '30 – Alteração de Dados Rejeitada';
      44: Result:= '44 – Título pago com cheque devolvido';
      50: Result:= '50 – Título pago com cheque pendente de compensação';
      85: Result:= '85 – Inclusão de Negativação';
      86: Result:= '86 – Exclusão de Negativação';
    end;
  end
  else
  begin
    case CodOcorrencia of
      02: Result:= '02-Confirmação de Entrada de Título';
      03: Result:= '03-Comando recusado';
      05: Result:= '05-Liquidado sem registro';
      06: Result:= '06-Liquidação Normal';
      07: Result:= '07-Liquidação por Conta';
      08: Result:= '08-Liquidação por Saldo';
      09: Result:= '09-Baixa de Título';
      10: Result:= '10-Baixa Solicitada';
      11: Result:= '11-Titulos em Ser';
      12: Result:= '12-Abatimento Concedido';
      13: Result:= '13-Abatimento Cancelado';
      14: Result:= '14-Alteração de Vencimento do Titulo';
      15: Result:= '15-Liquidação em Cartório';
      16: Result:= '16-Confirmação de alteração de juros de mora';
      17: Result:= '17-Liquidação Após Baixa ou Liquidação de Título Não Registrado';
      19: Result:= '19-Confirmação de recebimento de instruções para protesto';
      20: Result:= '20-Débito em Conta';
      21: Result:= '21-Alteração do Nome do Sacado';
      22: Result:= '22-Alteração do Endereço do Sacado';
      23: Result:= '23-Indicação de encaminhamento a cartório';
      24: Result:= '24-Sustar Protesto';
      25: Result:= '25-Dispensar Juros';
      26: Result:= '26-Alteração do número do título dado pelo Cedente (Seu número) - 10 e 15 posições';
      28: Result:= '28-Manutenção de titulo vencido';
      31: Result:= '31-Conceder desconto';
      32: Result:= '32-Não conceder desconto';
      33: Result:= '33-Retificar desconto';
      34: Result:= '34-Alterar data para desconto';
      35: Result:= '35-Cobrar multa';
      36: Result:= '36-Dispensar multa';
      37: Result:= '37-Dispensar indexador';
      38: Result:= '38-Dispensar prazo limite para recebimento';
      39: Result:= '39-Alterar prazo limite para recebimento';
      41: Result:= '41-Alteração do número do controle do participante (25 posições)';
      44: Result:= '44-Título pago com cheque devolvido';
      46: Result:= '46-Título pago com cheque, aguardando compensação';
      72: Result:= '72-Alteração de tipo de cobrança';
      85: Result:= '85 – Inclusão de Negativação';
      86: Result:= '86 – Exclusão de Negativação';
      96: Result:= '96-Despesas de Protesto';
      97: Result:= '97-Despesas de Sustação de Protesto';
      98: Result:= '98-Débito de Custas Antecipadas';
    end;
  end;

  Result := ACBrSTr(Result);
end;

function TACBrBancoBrasil.CodOcorrenciaToTipo(const CodOcorrencia:
   Integer ) : TACBrTipoOcorrencia;
begin
   Result := toTipoOcorrenciaNenhum;

  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
  begin
    case CodOcorrencia of
      03: Result := toRetornoRegistroRecusado;
      04: Result := toRetornoTransferenciaCarteiraEntrada;
      05: Result := toRetornoTransferenciaCarteiraBaixa;
      15: Result := toRetornoBaixadoFrancoPagamento;
      17: Result := toRetornoLiquidadoSemRegistro;
      20: Result := toRetornoRecebimentoInstrucaoSustarProtesto;
      24: Result := toRetornoRetiradoDeCartorio;
      25: Result := toRetornoBaixaPorProtesto;
      26: Result := toRetornoInstrucaoRejeitada;
      27: Result := toRetornoAlteracaoUsoCedente;
      28: Result := toRetornoDebitoTarifas;
      29: Result := toRetornoOcorrenciasDoSacado;
      30: Result := toRetornoAlteracaoDadosRejeitados;
      50: Result := toRetornoChequePendenteCompensacao;
      85: Result := toRetornoInclusaoNegativacao;
      86: Result := toRetornoExclusaoNegativacao;
    end;
  end
  else
  begin
    case CodOcorrencia of
      03: Result := toRetornoComandoRecusado;
      05: Result := toRetornoLiquidadoSemRegistro;
      07: Result := toRetornoLiquidadoPorConta;
      08: Result := toRetornoLiquidadoSaldoRestante;
      10: Result := toRetornoBaixaSolicitada;
      15: Result := toRetornoLiquidadoEmCartorio;
      16: Result := toRetornoConfirmacaoAlteracaoJurosMora;
      17: Result := toRetornoLiquidadoAposBaixaOuNaoRegistro;
      20: Result := toRetornoDebitoEmConta;
      21: Result := toRetornoRecebimentoInstrucaoAlterarNomeSacado;
      22: Result := toRetornoRecebimentoInstrucaoAlterarEnderecoSacado;
      23: Result := toRetornoEncaminhadoACartorio;
      24: Result := toRetornoProtestoSustado;
      25: Result := toRetornoJurosDispensados;
      28: Result := toRetornoManutencaoTituloVencido;
      31: Result := toRetornoDescontoConcedido;
      32: Result := toRetornoDescontoCancelado;
      33: Result := toRetornoDescontoRetificado;
      34: Result := toRetornoAlterarDataDesconto;
      35: Result := toRetornoRecebimentoInstrucaoAlterarJuros;
      36: Result := toRetornoRecebimentoInstrucaoDispensarJuros;
      37: Result := toRetornoDispensarIndexador;
      38: Result := toRetornoDispensarPrazoLimiteRecebimento;
      39: Result := toRetornoAlterarPrazoLimiteRecebimento;
      41: Result := toRetornoAcertoControleParticipante;
      46: Result := toRetornoChequePendenteCompensacao;
      72: Result := toRetornoTipoCobrancaAlterado;
      85: Result := toRetornoInclusaoNegativacao;
      86: Result := toRetornoExclusaoNegativacao;
      96: Result := toRetornoDespesasProtesto;
      97: Result := toRetornoDespesasSustacaoProtesto;
      98: Result := toRetornoDebitoCustasAntecipadas;
    end;
  end;

  if (Result <> toTipoOcorrenciaNenhum) then
    Exit;

  case CodOcorrencia of
    02: Result := toRetornoRegistroConfirmado;
    06: Result := toRetornoLiquidado;
    09: Result := toRetornoBaixaAutomatica;
    11: Result := toRetornoTituloEmSer;
    12: Result := toRetornoAbatimentoConcedido;
    13: Result := toRetornoAbatimentoCancelado;
    14: Result := toRetornoVencimentoAlterado;
    19: Result := toRetornoRecebimentoInstrucaoProtestar;
    23: Result := toRetornoEntradaEmCartorio;
    44: Result := toRetornoChequeDevolvido;
  else
    Result := toRetornoOutrasOcorrencias;
  end;
end;

function TACBrBancoBrasil.CodOcorrenciaToTipoRemessa(const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02 : Result:= toRemessaBaixar;                          {Pedido de Baixa}
    04 : Result:= toRemessaConcederAbatimento;              {Concessão de Abatimento}
    05 : Result:= toRemessaCancelarAbatimento;              {Cancelamento de Abatimento concedido}
    06 : Result:= toRemessaAlterarVencimento;               {Alteração de vencimento}
    07 : Result:= toRemessaAlterarControleParticipante;     {Alteração do controle do participante}
    08 : Result:= toRemessaAlterarNumeroControle;           {Alteração de seu número}
    09 : Result:= toRemessaProtestar;                       {Pedido de protesto}
    10 : Result:= toRemessaCancelarInstrucaoProtestoBaixa;  {Instrução para sustar protesto}
    11 : Result:= toRemessaDispensarJuros;                  {Instrução para dispensar juros}
    12 : Result:= toRemessaAlterarNomeEnderecoSacado;       {Alteração de nome e endereço do Sacado}
    31 : Result:= toRemessaOutrasOcorrencias;               {Alteração de Outros Dados}
    32 : Result:= toRemessaCancelarDesconto;                {Não conceder desconto}
    40 : Result:= toRemessaAlterarModalidade;               {Alterar modalidade (Vide Observações)}
  else
     Result:= toRemessaRegistrar;                           {Remessa}
  end;
end;

function TACBrBancoBrasil.CodMotivoRejeicaoToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: Integer): String;
begin
  Result := '';

  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c400) then
  begin
    case TipoOcorrencia of
    toRetornoComandoRecusado: //03 (Recusado)
      case CodMotivo of
        01: Result:='01-Identificação inválida' ;
        02: Result:='02-Variação da carteira inválida' ;
        03: Result:='03-Valor dos juros por um dia inválido' ;
        04: Result:='04-Valor do desconto inválido' ;
        05: Result:='05-Espécie de título inválida para carteira' ;
        06: Result:='06-Espécie de valor variável inválido' ;
        07: Result:='07-Prefixo da agência usuária inválido' ;
        08: Result:='08-Valor do título/apólice inválido' ;
        09: Result:='09-Data de vencimento inválida' ;
        10: Result:='10-Fora do prazo' ;
        11: Result:='11-Inexistência de margem para desconto' ;
        12: Result:='12-O Banco não tem agência na praça do sacado' ;
        13: Result:='13-Razões cadastrais' ;
        14: Result:='14-Sacado interligado com o sacador' ;
        15: Result:='15-Título sacado contra orgão do Poder Público' ;
        16: Result:='16-Título preenchido de forma irregular' ;
        17: Result:='17-Título rasurado' ;
        18: Result:='18-Endereço do sacado não localizado ou incompleto' ;
        19: Result:='19-Código do cedente inválido' ;
        20: Result:='20-Nome/endereco do cliente não informado /ECT/' ;
        21: Result:='21-Carteira inválida' ;
        22: Result:='22-Quantidade de valor variável inválida' ;
        23: Result:='23-Faixa nosso número excedida' ;
        24: Result:='24-Valor do abatimento inválido' ;
        25: Result:='25-Novo número do título dado pelo cedente inválido' ;
        26: Result:='26-Valor do IOF de seguro inválido' ;
        27: Result:='27-Nome do sacado/cedente inválido ou não informado' ;
        28: Result:='28-Data do novo vencimento inválida' ;
        29: Result:='29-Endereco não informado' ;
        30: Result:='30-Registro de título já liquidado' ;
        31: Result:='31-Numero do bordero inválido' ;
        32: Result:='32-Nome da pessoa autorizada inválido' ;
        33: Result:='33-Nosso número já existente' ;
        34: Result:='34-Numero da prestação do contrato inválido' ;
        35: Result:='35-Percentual de desconto inválido' ;
        36: Result:='36-Dias para fichamento de protesto inválido' ;
        37: Result:='37-Data de emissão do título inválida' ;
        38: Result:='38-Data do vencimento anterior a data da emissão do título' ;
        39: Result:='39-Comando de alteração indevido para a carteira' ;
        40: Result:='40-Tipo de moeda inválido' ;
        41: Result:='41-Abatimento não permitido' ;
        42: Result:='42-CEP do sacado inválido /ECT/' ;
        43: Result:='43-Codigo de unidade variavel incompativel com a data emissão do título' ;
        44: Result:='44-Dados para debito ao sacado inválidos' ;
        45: Result:='45-Carteira' ;
        46: Result:='46-Convenio encerrado' ;
        47: Result:='47-Título tem valor diverso do informado' ;
        48: Result:='48-Motivo de baixa inválido para a carteira' ;
        49: Result:='49-Abatimento a cancelar não consta do título' ;
        50: Result:='50-Comando incompativel com a carteira' ;
        51: Result:='51-Codigo do convenente inválido' ;
        52: Result:='52-Abatimento igual ou maior que o valor do título' ;
        53: Result:='53-Título já se encontra situação pretendida' ;
        54: Result:='54-Título fora do prazo admitido para a conta 1' ;
        55: Result:='55-Novo vencimento fora dos limites da carteira' ;
        56: Result:='56-Título não pertence ao convenente' ;
        57: Result:='57-Variação incompativel com a carteira' ;
        58: Result:='58-Impossivel a transferencia para a carteira indicada' ;
        59: Result:='59-Título vencido em transferencia para a carteira 51' ;
        60: Result:='60-Título com prazo superior a 179 dias em transferencia para carteira 51' ;
        61: Result:='61-Título já foi fichado para protesto' ;
        62: Result:='62-Alteração da situação de debito inválida para o codigo de responsabilidade' ;
        63: Result:='63-DV do nosso número inválido' ;
        64: Result:='64-Título não passivel de debito/baixa - situação anormal' ;
        65: Result:='65-Título com ordem de não protestar-não pode ser encaminhado a cartorio' ;
        66: Result:='66-Número do documento do sacado (CNPJ/CPF) inválido';
        67: Result:='67-Título/carne rejeitado';
        68: Result:='68-Código/Data/Percentual de multa inválido';
        69: Result:='69-Valor/Percentual de Juros Inválido';
        70: Result:='70-Título já se encontra isento de juros';
        71: Result:='71-Código de Juros Inválido';
        72: Result:='72-Prefixo da Ag. cobradora inválido';
        73: Result:='73–Numero do controle do participante inválido';
        74: Result:='74–Cliente não cadastrado no CIOPE (Desconto/Vendor)';
        75: Result:='75–Qtde. de dias do prazo limite p/ recebimento de título vencido inválido';
        76: Result:='76–Titulo excluído automaticamente por decurso deprazo CIOPE (Desconto/Vendor)';
        77: Result:='77–Titulo vencido transferido para a conta 1 – Carteira vinculada';
        80: Result:='80-Nosso número inválido' ;
        81: Result:='81-Data para concessão do desconto inválida' ;
        82: Result:='82-CEP do sacado inválido' ;
        83: Result:='83-Carteira/variação não localizada no cedente' ;
        84: Result:='84-Título não localizado na existencia' ;
        99: Result:='99-Outros motivos' ;
      end;
    toRetornoLiquidadoSemRegistro, 
    toRetornoLiquidado, 
    toRetornoLiquidadoPorConta,
    toRetornoLiquidadoSaldoRestante,
    toRetornoLiquidadoEmCartorio,
    toRetornoChequePendenteCompensacao: // 05, 06, 07, 08 , 15 e 46 (Liquidado)
      case CodMotivo of
        01: Result:='01-Liquidação normal';
        02: Result:='02-Liquidação parcial';
        03: Result:='03-Liquidação por saldo';
        04: Result:='04-Liquidação com cheque a compensar';
        05: Result:='05-Liquidação de título sem registro (carteira 7 tipo 4)';
        07: Result:='07-Liquidação na apresentação';
        09: Result:='09-Liquidação em cartório';
      end;
    toRetornoTituloPagoEmCheque,    // 46–Título pago com cheque, aguardando compensação
    toRetornoTipoCobrancaAlterado:  // 72 (Tipo de Cobrança)
      case CodMotivo of
        00: Result := '00-Transferência de título de cobrança simples para descontada ou vice-versa';
        52: Result := '52-Reembolso de título vendor ou descontado';
      end;
    toRetornoConfirmacaoRecebPedidoNegativacao, toRetornoInclusaoNegativacao: //  85 – Inclusão de Negativação (Particularidades BB jan/2019)
      case CodMotivo of
        01: Result:='01-Negativação aceita no BB';
        02: Result:='02-Negativação aceita no agente negativador';
        03: Result:='03-Inclusão cancelada';
        04: Result:='04-Negativação recusada - pagador menor de idade';
        05: Result:='05-Negativação recusada - espécie do boleto não permitida';
        06: Result:='06-Negativação recusada - beneficiário não é PJ';
        07: Result:='07-Negativação recusada - moeda do boleto não é Real';
        08: Result:='08-Negativação recusada - endereço do pagador inválido';
        09: Result:='09-Negativação recusada pelo agente negativado';
        10: Result:='10-Negativação recusada - situação do boleto não permite NGTV';
        11: Result:='11-Negativação recusada - cadastro do benef. desatualizado';
        12: Result:='12-Negativação recusada - boleto inexistente';
        13: Result:='13-Negativação recusada - pagador não identificado';
        14: Result:='14-Recusa de tarifação de negativação';
        15: Result:='15-Negativação recusada - motivos diversos';
      end;
    toRetornoConfirmacaoPedidoExclNegativacao, toRetornoExclusaoNegativacao: //  86 – Exclusão de Negativação (Particularidades BB jan/2019)
      case CodMotivo of
        01: Result:='01-Exclusão cancelada';
        02: Result:='02-Negativação excluída no agente negativador';
        03: Result:='03-Negativação excluída - devolução pelos correios';
        04: Result:='04-Negativação excluída - data de ocorrência decursada';
        05: Result:='05-Negativação excluída - determinação judicial';
        06: Result:='06-Negativação excluída - contestação do interessado';
        07: Result:='07-Negativação excluída - carta não retornou do correio';
        08: Result:='08-Exclusão negativação recusada - registro inexistente';
        15: Result:='09-Exclusão negativação recusada - motivos diversos';
      end;

    else
      Result := IntToStrZero(CodMotivo, 2) + ' - Outros Motivos';
    end;
  end
  else
  begin
    case TipoOcorrencia of
    toRetornoRegistroRecusado: // 03 (Recusado)
      case CodMotivo of
        01: Result:='01-Codigo do banco invalido';
        02: Result:='02-Codigo do registro detalhe invalido';
        03: Result:='03-Codigo do segmento invalido';
        04: Result:='04-Codigo do movimento nao permitido para carteira';
        05: Result:='05-Codigo de movimento invalido';
        06: Result:='06-Tipo/numero de inscricao do cedente Invalidos';
        07: Result:='07-Agencia/Conta/DV invalido';
        08: Result:='08-Nosso numero invalido';
        09: Result:='09-Nosso numero duplicado';
        10: Result:='10-Carteira invalida';
        11: Result:='11-Forma de cadastramento do titulo invalido';
        12: Result:='12-Tipo de documento invalido';
        13: Result:='13-Identificacao da emissao do bloqueto invalida';
        14: Result:='14-Identificacao da distribuicao do bloqueto invalida';
        15: Result:='15-Caracteristicas da cobranca incompativeis';
        16: Result:='16-Data de vencimento invalida';
        17: Result:='17-Data de vencimento anterior a data de emissao';
        18: Result:='18-Vencimento fora do prazo de operacao';
        19: Result:='19-Titulo a cargo de Bancos Correspondentes com vencimento inferior XX dias';
        20: Result:='20-Valor do titulo invalido';
        21: Result:='21-Especie do titulo invalida';
        22: Result:='22-Especie nao permitida para a carteira';
        23: Result:='23-Aceite invalido';
        24: Result:='24-Data da emissao invalida';
        25: Result:='25-Data da emissao posterior a data';
        26: Result:='26-Codigo de juros de mora invalido';
        27: Result:='27-Valor/Taxa de juros de mora invalido';
        28: Result:='28-Codigo do desconto invalido';
        29: Result:='29-Valor do desconto maior ou igual ao valor do titulo ';
        30: Result:='30-Desconto a conceder nao confere';
        31: Result:='31-Concessao de desconto - ja existe desconto anterior';
        32: Result:='32-Valor do IOF invalido';
        33: Result:='33-Valor do abatimento invalido';
        34: Result:='34-Valor do abatimento maior ou igual ao valor do titulo';
        35: Result:='35-Abatimento a conceder nao confere';
        36: Result:='36-Concessao de abatimento - ja existe abatimento anterior';
        37: Result:='37-Codigo para protesto invalido';
        38: Result:='38-Prazo para protesto invalido';
        39: Result:='39-Pedido de protesto nao permitido para o titulo';
        40: Result:='40-Titulo com ordem de protesto emitida';
        41: Result:='41-Pedido de cancelamento/sustacao para titulos sem instrucao de protesto';
        42: Result:='42-Codigo para baixa/devolucao invalido';
        43: Result:='43-Prazo para baixa/devolucao invalido';
        44: Result:='44-Codigo da moeda invalido';
        45: Result:='45-Nome do sacado nao informado';
        46: Result:='46-Tipo/numero de inscricao do sacado invalidos';
        47: Result:='47-Endereco do sacado nao informado';
        48: Result:='48-CEP invalido';
        49: Result:='49-CEP sem praca de cobranca /nao localizado';
        50: Result:='50-CEP referente a um Banco Correspondente';
        51: Result:='51-CEP incompativel com a unidade da federacao';
        52: Result:='52-Unidade da federacao invalida';
        53: Result:='53-Tipo/numero de inscricao do sacador/avalista invalidos';
        54: Result:='54-Sacador/Avalista nao informado';
        55: Result:='55-Nosso numero no Banco Correspondente nao informado';
        56: Result:='56-Codigo do Banco Correspondente nao informado';
        57: Result:='57-Codigo da multa invalido';
        58: Result:='58-Data da multa invalida';
        59: Result:='59-Valor/Percentual da multa invalido';
        60: Result:='60-Movimento para titulo nao cadastrado';
        61: Result:='61-Alteracao da agencia cobradora/dv invalida';
        62: Result:='62-Tipo de impressao invalido';
        63: Result:='63-Entrada para titulo ja cadastrado';
        64: Result:='64-Numero da linha invalido';
        65: Result:='65-Codigo do banco para debito invalido';
        66: Result:='66-Agencia/conta/DV para debito invalido';
        67: Result:='67-Dados para debito incompativel com a identificacao da emissao do bloqueto';
        88: Result:='88-Arquivo em duplicidade';
        99: Result:='99-Contrato inexistente';
        101 { A1 } : Result := 'Rejeição da alteração do número controle do participante';
        102 { A2 } : Result := 'Rejeição da alteração dos dados do Pagador';
        103 { A3 } : Result := 'Rejeição da alteração dos dados do Sacador/avalista';
        104 { A4 } : Result := 'Pagador DDA';
        105 { A5 } : Result := 'Registro Rejeitado – Título já Liquidado';
        106 { A6 } : Result := 'Código do Convenente Inválido ou Encerrado';
        107 { A7 } : Result := 'Título já se encontra na situação Pretendida';
        108 { A8 } : Result := 'Valor do Abatimento inválido para cancelamento';
        109 { A9 } : Result := 'Não autoriza pagamento parcial';
      end;
      toRetornoLiquidado, toRetornoBaixaAutomatica, toRetornoLiquidadoSemRegistro: // 06, 09 e 17 (Liquidado)
      case CodMotivo of
        01: Result:='01-Por saldo';
        02: Result:='02-Parcial';
        03: Result:='03-No proprio banco';
        04: Result:='04-Compensacao eletronica';
        05: Result:='05-Compensacao convencional';
        06: Result:='06-Por meio eletronico';
        07: Result:='07-Apos feriado local';
        08: Result:='08-Em cartorio';
        30: Result:='30-Liquidação no Guichê de Caixa em cheque';
        09: Result:='09-Comandada banco';
        10: Result:='10-Comandada cliente arquivo';
        11: Result:='11-Comandada cliente on-line';
        12: Result:='12-Decurso prazo - cliente';
        13: Result:='13-Decurso prazo - banco';
        99: Result:='99-Liquidado por agendamento';
      end;
      toRetornoDebitoTarifas: // 28 - Débito de Tarifas/Custas (Febraban 240 posições, v08.9 de 15/04/2014)
      case CodMotivo of
        01: Result:='01-Tarifa de Extrato de Posição';
        02: Result:='02-Tarifa de Manutenção de Título Vencido';
        03: Result:='03-Tarifa de Sustação';
        04: Result:='04-Tarifa de Protesto';
        05: Result:='05-Tarifa de Outras Instruções';
        06: Result:='06-Tarifa de Outras Ocorrências';
        07: Result:='07-Tarifa de Envio de Duplicata ao Sacado';
        08: Result:='08-Custas de Protesto';
        09: Result:='09-Custas de Sustação de Protesto';
        10: Result:='10-Custas de Cartório Distribuidor';
        11: Result:='11-Custas de Edital';
        12: Result:='12-Tarifa Sobre Devolução de Título Vencido';
        13: Result:='13-Tarifa Sobre Registro Cobrada na Baixa/Liquidação';
        14: Result:='14-Tarifa Sobre Reapresentação Automática';
        15: Result:='15-Tarifa Sobre Rateio de Crédito';
        16: Result:='16-Tarifa Sobre Informações Via Fax';
        17: Result:='17-Tarifa Sobre Prorrogação de Vencimento';
        18: Result:='18-Tarifa Sobre Alteração de Abatimento/Desconto';
        19: Result:='19-Tarifa Sobre Arquivo mensal (Em Ser)';
        20: Result:='20-Tarifa Sobre Emissão de Bloqueto Pré-Emitido pelo Banco';
      end;
      toRetornoConfirmacaoRecebPedidoNegativacao, toRetornoInclusaoNegativacao: //  85 – Inclusão de Negativação (Particularidades BB jan/2019)
      case CodMotivo of
        01: Result:='01-Negativação aceita no BB';
        02: Result:='02-Negativação aceita no agente negativador';
        03: Result:='03-Inclusão cancelada';
        04: Result:='04-Negativação recusada - pagador menor de idade';
        05: Result:='05-Negativação recusada - espécie do boleto não permitida';
        06: Result:='06-Negativação recusada - beneficiário não é PJ';
        07: Result:='07-Negativação recusada - moeda do boleto não é Real';
        08: Result:='08-Negativação recusada - endereço do pagador inválido';
        09: Result:='09-Negativação recusada pelo agente negativado';
        10: Result:='10-Negativação recusada - situação do boleto não permite NGTV';
        11: Result:='11-Negativação recusada - cadastro do benef. desatualizado';
        12: Result:='12-Negativação recusada - boleto inexistente';
        13: Result:='13-Negativação recusada - pagador não identificado';
        14: Result:='14-Recusa de tarifação de negativação';
        15: Result:='15-Negativação recusada - motivos diversos';
      end;
      toRetornoConfirmacaoPedidoExclNegativacao, toRetornoExclusaoNegativacao: //  86 – Exclusão de Negativação (Particularidades BB jan/2019)
      case CodMotivo of
        01: Result:='01-Exclusão cancelada';
        02: Result:='02-Negativação excluída no agente negativador';
        03: Result:='03-Negativação excluída - devolução pelos correios';
        04: Result:='04-Negativação excluída - data de ocorrência decursada';
        05: Result:='05-Negativação excluída - determinação judicial';
        06: Result:='06-Negativação excluída - contestação do interessado';
        07: Result:='07-Negativação excluída - carta não retornou do correio';
        08: Result:='08-Exclusão negativação recusada - registro inexistente';
        15: Result:='09-Exclusão negativação recusada - motivos diversos';
      end;
    else
       Result := IntToStrZero(CodMotivo, 2) + ' - Outros Motivos';
    end;
    case TipoOcorrencia of
    toRetornoRegistroConfirmado:       //02 (Entrada)
      case CodMotivo of
        00: Result:='00-Por meio magnético';
        11: Result:='11-Por via convencional';
        16: Result:='16-Por alteração do código do cedente';
        17: Result:='17-Por alteração da variação';
        18: Result:='18-Por alteração de carteira';
      end;
    toRetornoBaixaAutomatica, 
    toRetornoBaixaSolicitada, 
    toRetornoDebitoEmConta: // 09, 10 ou 20 (Baixa)
      case CodMotivo of
        00: Result:='00-Solicitada pelo cliente';
        14: Result:='14-Protestado';
        15:
          case ACBrBanco.ACBrBoleto.LayoutRemessa of
            c240: Result := '15-Título Excluído';
            c400: Result := '15-Protestado';
          end;
        18: Result:='18-Por alteração de carteira';
        19: Result:='19-Débito automático';
        31: Result:='31-Liquidado anteriormente';
        32: Result:='32-Habilitado em processo';
        33: Result:='33-Incobrável por nosso intermédio';
        34: Result:='34-Transferido para créditos em liquidação';
        46: Result:='46-Por alteração da variação';
        47: Result:='47-Por alteração da variação';
        51: Result:='51-Acerto';
        90: Result:='90-Baixa automática';
      end;
    end;
  end;

  Result := ACBrSTr(Result);
end;

procedure TACBrBancoBrasil.LerRetorno400(ARetorno: TStringList);
var
 TamConvenioMaior6: Boolean;
 rConvenioLider : String;
begin

 if ACBrBanco.ACBrBoleto.LeCedenteRetorno then
 begin
   if NaoEstaZerado(StrToInt64Def(Copy(ARetorno[0],41,6),0)) then
     rConvenioLider := Trim(Copy(ARetorno[0],41,6))//CBR643 convenio 6 posicoes
   else
     rConvenioLider := Copy(ARetorno[0],150,7); //CBR643 convenio 7 posicoes
   ACBrBanco.ACBrBoleto.Cedente.Convenio := rConvenioLider;
 end;
 TamConvenioMaior6:= Length(trim(ACBrBanco.ACBrBoleto.Cedente.Convenio)) > 6;
 if TamConvenioMaior6 then
    LerRetorno400Pos7(ARetorno)
 else
    LerRetorno400Pos6(ARetorno);
end;

procedure TACBrBancoBrasil.LerRetorno400Pos6(ARetorno: TStringList);
var
  Titulo : TACBrTitulo;
  ContLinha, CodOcorrencia, CodMotivo, MotivoLinha : Integer;
  rAgencia, rDigitoAgencia, rConta :String;
  rDigitoConta, rCodigoCedente     :String;
  Linha, rCedente                  :String;
  rConvenioCedente: String;
begin
   fpTamanhoMaximoNossoNum := 11;

   if StrToIntDef(copy(ARetorno.Strings[0],77,3),-1) <> Numero then
     raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                                    'não é um arquivo de retorno do '+ Nome));

   rCedente      := trim(Copy(ARetorno[0],47,30));
   rAgencia      := trim(Copy(ARetorno[0],27,4));
   rDigitoAgencia:= Copy(ARetorno[0],31,1);
   rConta        := trim(Copy(ARetorno[0],32,8));
   rDigitoConta  := Copy(ARetorno[0],40,1);

   rCodigoCedente  := Copy(ARetorno[0],41,6);
   rConvenioCedente:= Copy(ARetorno[0],41,6);


   ACBrBanco.ACBrBoleto.NumeroArquivo := StrToIntDef(Copy(ARetorno[0],101,7),0);

   ACBrBanco.ACBrBoleto.DataArquivo   := StringToDateTimeDef(Copy(ARetorno[0],95,2)+'/'+
                                                             Copy(ARetorno[0],97,2)+'/'+
                                                             Copy(ARetorno[0],99,2),0, 'DD/MM/YY' );

   ValidarDadosRetorno(rAgencia, rConta);
   with ACBrBanco.ACBrBoleto do
   begin
     if LeCedenteRetorno then
     begin
       Cedente.Nome         := rCedente;
       Cedente.Agencia      := rAgencia;
       Cedente.AgenciaDigito:= rDigitoAgencia;
       Cedente.Conta        := rConta;
       Cedente.ContaDigito  := rDigitoConta;
       Cedente.CodigoCedente:= rCodigoCedente;
       Cedente.Convenio     := rConvenioCedente;
     end;

     ACBrBanco.ACBrBoleto.ListadeBoletos.Clear;
   end;

   ACBrBanco.TamanhoMaximoNossoNum := fpTamanhoMaximoNossoNum;

   for ContLinha := 1 to ARetorno.Count - 2 do
   begin
     Linha := ARetorno[ContLinha] ;

     if (Copy(Linha,1,1) <> '1') then
       Continue;

     Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

     with Titulo do
     begin
       SeuNumero               := copy(Linha,38,25);
       NumeroDocumento         := copy(Linha,117,10);

       CodOcorrencia := StrToIntDef(copy(Linha,109,2),0);
       OcorrenciaOriginal.Tipo := CodOcorrenciaToTipo(CodOcorrencia);

       if ((CodOcorrencia >= 5) and (CodOcorrencia <= 8)) or
          (CodOcorrencia = 15) or (CodOcorrencia = 46) then
       begin
         CodigoLiquidacao := IntToStr(CodOcorrencia);
         CodigoLiquidacaoDescricao := TipoOcorrenciaToDescricao(OcorrenciaOriginal.Tipo);
       end;

       if ((CodOcorrencia >= 2) and (CodOcorrencia <= 10)) or ( CodOcorrencia >= 85 )then
       begin
         MotivoLinha:= 81;
         CodMotivo:= StrToInt(copy(Linha,MotivoLinha,2));
         MotivoRejeicaoComando.Add(copy(Linha,MotivoLinha,2));
         DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo,CodMotivo));
       end;

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
       ValorRecebido        := StrToFloatDef(Copy(Linha,254,13),0)/100;
       ValorMoraJuros       := StrToFloatDef(Copy(Linha,267,13),0)/100;
       ValorOutrosCreditos  := StrToFloatDef(Copy(Linha,280,13),0)/100;
       Carteira             := Copy(Linha,107,2);
	   
       case CalcularTamMaximoNossoNumero(Carteira, '', rConvenioCedente) of
         11: NossoNumero := Copy(Linha,63,11);
         else NossoNumero := Copy(Linha,69,5);
       end;

       ValorDespesaCobranca := StrToFloatDef(Copy(Linha,182,07),0)/100;
       ValorOutrasDespesas  := StrToFloatDef(Copy(Linha,189,13),0)/100;

       if StrToIntDef(Copy(Linha,176,6),0) <> 0 then
         DataCredito:= StringToDateTimeDef( Copy(Linha,176,2)+'/'+
                                            Copy(Linha,178,2)+'/'+
                                            Copy(Linha,180,2),0, 'DD/MM/YY' );
     end;
   end;

   fpTamanhoMaximoNossoNum := 10;
end;

procedure TACBrBancoBrasil.LerRetorno400Pos7(ARetorno: TStringList);
var
  Titulo : TACBrTitulo;
  ContLinha, CodOcorrencia, CodMotivo : Integer;
  rAgencia, rDigitoAgencia, rConta :String;
  rDigitoConta, rCodigoCedente     :String;
  Linha, rCedente                  :String;
  rConvenioCedente: String;
begin
  fpTamanhoMaximoNossoNum := 20;

  if StrToIntDef(copy(ARetorno.Strings[0],77,3),-1) <> Numero then
    raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                                   'não é um arquivo de retorno do '+ Nome));

  rCedente      := trim(Copy(ARetorno[0],47,30));
  rAgencia      := trim(Copy(ARetorno[0],27,4));
  rDigitoAgencia:= Copy(ARetorno[0],31,1);
  rConta        := trim(Copy(ARetorno[0],32,8));
  rDigitoConta  := Copy(ARetorno[0],40,1);

  rCodigoCedente  := Copy(ARetorno[0],150,7);
  rConvenioCedente:= Copy(ARetorno[0],150,7);


  ACBrBanco.ACBrBoleto.NumeroArquivo := StrToIntDef(Copy(ARetorno[0],101,7),0);

  ACBrBanco.ACBrBoleto.DataArquivo   := StringToDateTimeDef(Copy(ARetorno[0],95,2)+'/'+
                                                            Copy(ARetorno[0],97,2)+'/'+
                                                            Copy(ARetorno[0],99,2),0, 'DD/MM/YY' );

  ValidarDadosRetorno(rAgencia, rConta);
  with ACBrBanco.ACBrBoleto do
  begin

    if LeCedenteRetorno then
    begin
      Cedente.Nome         := rCedente;
      Cedente.Agencia      := rAgencia;
      Cedente.AgenciaDigito:= rDigitoAgencia;
      Cedente.Conta        := rConta;
      Cedente.ContaDigito  := rDigitoConta;
      Cedente.CodigoCedente:= rCodigoCedente;
      Cedente.Convenio     := rConvenioCedente;
    end;

    ACBrBanco.ACBrBoleto.ListadeBoletos.Clear;
  end;

  ACBrBanco.TamanhoMaximoNossoNum := 20;

  for ContLinha := 1 to ARetorno.Count - 2 do
  begin
    Linha := ARetorno[ContLinha] ;

    if (Copy(Linha,1,1) <> '7') and (Copy(Linha,1,1) <> '1') then
      Continue;

    Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

    with Titulo do
    begin
      SeuNumero       := copy(Linha,39,25);
      NumeroDocumento := copy(Linha,117,10);
      CodOcorrencia   := StrToIntDef(copy(Linha,109,2),0);
      OcorrenciaOriginal.Tipo := CodOcorrenciaToTipo(CodOcorrencia);

      if ((CodOcorrencia >= 5) and (CodOcorrencia <= 8)) or
          (CodOcorrencia = 15) or (CodOcorrencia = 46) then
      begin
        CodigoLiquidacao := copy(Linha,109,2);
        CodigoLiquidacaoDescricao := TipoOcorrenciaToDescricao(OcorrenciaOriginal.Tipo);
      end;

      if(CodOcorrencia >= 2) and ((CodOcorrencia <= 10)) then
      begin

        CodMotivo:= StrToIntDef(Copy(Linha,87,2),0);
        MotivoRejeicaoComando.Add(copy(Linha,87,2));
        DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo,CodMotivo));
      end;

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
      ValorRecebido        := StrToFloatDef(Copy(Linha,254,13),0)/100;
      ValorMoraJuros       := StrToFloatDef(Copy(Linha,267,13),0)/100;
      ValorOutrosCreditos  := StrToFloatDef(Copy(Linha,280,13),0)/100;
      Carteira             := Copy(Linha,107,2);
      NossoNumero          := Copy(Linha,71,10);
      ValorDespesaCobranca := StrToFloatDef(Copy(Linha,182,07),0)/100;
      ValorOutrasDespesas  := StrToFloatDef(Copy(Linha,189,13),0)/100;

      if StrToIntDef(Copy(Linha,176,6),0) <> 0 then
        DataCredito:= StringToDateTimeDef( Copy(Linha,176,2)+'/'+
                                           Copy(Linha,178,2)+'/'+
                                           Copy(Linha,180,2),0, 'DD/MM/YY' );
    end;
  end;
  fpTamanhoMaximoNossoNum := 10;
end;



end.
