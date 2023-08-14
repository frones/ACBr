{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Paulo Monteiro                                  }
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
unit ACBrBancoAmazonia;

interface

uses
     Classes, SysUtils, Contnrs, ACBrBoleto, ACBrBoletoConversao;

const
     CACBrBancoAmazonia_Versao = '0.0.1';

type
     { TACBrBancoAmazonia }

     TACBrBancoAmazonia = class(TACBrBancoClass)
     protected
     private
          function FormataNossoNumero(const ACBrTitulo: TACBrTitulo): String;
          procedure LerRetorno400Pos6(ARetorno: TStringList);
          procedure LerRetorno400Pos7(ARetorno: TStringList);
     public
          Constructor create(AOwner: TACBrBanco);
          function CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo): String; override;
          function MontarCodigoBarras(const ACBrTitulo: TACBrTitulo): String; override;
          function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String; override;
          function MontarCampoNossoNumero(const ACBrTitulo: TACBrTitulo): String; override;
          function GerarRegistroHeader240(NumeroRemessa: Integer): String; override;
          function GerarRegistroTransacao240(ACBrTitulo: TACBrTitulo): String; override;
          function GerarRegistroTrailler240(ARemessa: TStringList): String; override;
          procedure GerarRegistroHeader400(NumeroRemessa: Integer; ARemessa: TStringList); override;
          procedure GerarRegistroTransacao400(ACBrTitulo: TACBrTitulo; ARemessa: TStringList); override;
          procedure GerarRegistroTrailler400(ARemessa: TStringList); override;
          function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String; override;
          function CodOcorrenciaToTipo(const CodOcorrencia: Integer): TACBrTipoOcorrencia; override;
          function TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia): String; override;
          Procedure LerRetorno240(ARetorno: TStringList); override;
          procedure LerRetorno400(ARetorno: TStringList); override;
          function CodMotivoRejeicaoToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: Integer): String; override;
          function CalcularTamMaximoNossoNumero(const Carteira: String; const NossoNumero: String = ''; const Convenio: String = ''): Integer; override;
          function CalcularNomeArquivoRemessa : String; override;
     end;

implementation

uses {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF}, StrUtils, Variants,
   ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings, ACBrUtil.DateTime;

function Modulo11BB(const AValor: String; Base: Integer = 9): string;
{
  Rotina muito usada para calcular dígitos verificadores
  Pega-se cada um dos dígitos contidos no parâmetro VALOR, da direita para a
  esquerda e multiplica-se pela seqüência de pesos 2, 3, 4 ... até BASE.
  Por exemplo: se a base for 9, os pesos serão 2,3,4,5,6,7,8,9,2,3,4,5...
  Se a base for 7, os pesos serão 2,3,4,5,6,7,2,3,4...
  Soma-se cada um dos subprodutos.
  Divide-se a soma por 11.
  Faz-se a operação 11-Resto da divisão e devolve-se o resultado dessa operação
  como resultado da função Modulo11.
  Obs.: Caso o resultado seja maior que 9, deverá ser substituído por 0 (ZERO).
}
     function Converte(APos: SmallInt; AMultiplicador: SmallInt): Integer;
     begin
          Result := StrToIntDef(Copy(AValor, APos, 1), 0) * AMultiplicador;
     end;

begin
     Result := IntToStr((Converte(1, 7) + Converte(2, 8) + Converte(3, 9) + Converte(4, 2) +
               Converte(5, 3) + Converte(6, 4) + Converte(7, 5) + Converte(8, 6) + Converte(9, 7) +
               Converte(10, 8) + Converte(11, 9)) mod 11);
end;

constructor TACBrBancoAmazonia.Create(AOwner: TACBrBanco);
begin
     inherited create(AOwner);
     fpDigito := 5;
     fpNome := 'Banco da Amazônia';
     fpNumero := 003;
     fpTamanhoMaximoNossoNum := 16;
     fpTamanhoConta := 7;
     fpTamanhoAgencia := 3;
     fpTamanhoCarteira := 3;
     fpCodigosMoraAceitos := '123';
end;

function TACBrBancoAmazonia.CalcularNomeArquivoRemessa: String;
var
  Sequencia   :Integer;
  NomeFixo,    NomeArq, sData , sIndiRemessa : String;

const
 sIndicadorFixo = 'VAN_AMZ';
begin

   with ACBrBanco.ACBrBoleto do
   begin
      case LayoutRemessa  of
        c400 : sIndiRemessa := '_CNAB400REM';
        c240 : sIndiRemessa := '_CNAB240REM';
      end;

      if NomeArqRemessa <> '' then
         Result := DirArqRemessa + PathDelim + NomeArqRemessa
      else
       begin

         sData := FormatDateTime('yyyymmdd',Now);


         NomeFixo := DirArqRemessa +
                     PathDelim +
                     sIndicadorFixo +
                     sData +
                     PadLeft(Cedente.CodigoCedente,10,'0') +
                     sIndiRemessa
                     ;

         NomeArq := NomeFixo + '.001';

         Sequencia := 1;
         while FilesExists(NomeArq) do
         begin
           Inc(Sequencia);
           NomeArq := NomeFixo + '.' + PadLeft(IntToStr(Sequencia),3,'0');

         end;

         Result:= NomeArq;
       end;
   end;
end;



function TACBrBancoAmazonia.CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo): String;
var
     ANossoNumero, ADigitoNossoNumero: string;
begin
     Result := '0';
     //
     with ACBrTitulo do
     begin
          if Carteira = '001' Then
           ANossoNumero := PadRight(OnlyNumber(NossoNumero), fpTamanhoMaximoNossoNum)
          else
           ANossoNumero := PadLeft(OnlyNumber(NossoNumero), fpTamanhoMaximoNossoNum, '0');

          ADigitoNossoNumero := Modulo11BB(ANossoNumero, 9);
     end;
     //
     Case StrToInt(ADigitoNossoNumero) of
          0:
               ADigitoNossoNumero := '0';
          1:
               ADigitoNossoNumero := 'X';
     else
          ADigitoNossoNumero := IntToStr(11 - StrToInt(ADigitoNossoNumero));
     end;
     //
     Result := ADigitoNossoNumero;
end;

function TACBrBancoAmazonia.CalcularTamMaximoNossoNumero(const Carteira: String; const NossoNumero: String = ''; const Convenio: String = ''): Integer;
var
     wTamConvenio: Integer;
begin
     if Carteira = '001' Then
      Result := 20 // carteira com registro possui 20 posições
     else
      Result := 10;

     if (ACBrBanco.ACBrBoleto.Cedente.Convenio = '') then
          raise Exception.create(ACBrStr('Banco da Amazonia requer que o número do convênio seja informado.'));

     wTamConvenio := Length(Trim(ACBrBanco.ACBrBoleto.Cedente.Convenio));

     if (wTamConvenio <= 4) and (Result <> 20) then
     Result := 16;
end;

function TACBrBancoAmazonia.FormataNossoNumero(const ACBrTitulo: TACBrTitulo): String;
var
     ANossoNumero: String;
     wTamNossoNum: Integer;
begin
     with ACBrTitulo do
     begin
          wTamNossoNum := CalcularTamMaximoNossoNumero(Carteira, NossoNumero);
          if Carteira = '001' Then
            ANossoNumero := PadRight(NossoNumero, wTamNossoNum)
          else
            ANossoNumero := PadLeft(NossoNumero, wTamNossoNum, '0')
     end;

     Result := ANossoNumero;
end;

function TACBrBancoAmazonia.MontarCodigoBarras(const ACBrTitulo: TACBrTitulo): String;
var
     CodigoBarras, FatorVencimento, DigitoCodBarras : String;
begin

     with ACBrTitulo.ACBrBoleto do
     begin
          FatorVencimento := CalcularFatorVencimento(ACBrTitulo.Vencimento);

          if ACBrTitulo.Carteira = '001' Then
            CodigoBarras := PadLeft(OnlyNumber(IntToStr(fpNumero)), 3, '0') +
                            '9' +
                            PadLeft(FatorVencimento,4,'0') +
                            IntToStrZero(Round(ACBrTitulo.ValorDocumento * 100), 10) +
                            Cedente.Agencia +
                            Cedente.AgenciaDigito +
                            RightStr(OnlyNumber(ACBrTitulo.NossoNumero),7) +
                            FormatDateTime('DDMMYY',ACBrTitulo.Vencimento) +
                            '00000000'
          else
            CodigoBarras := PadLeft(OnlyNumber(IntToStr(fpNumero)), 3, '0') +
                            '9' +
                            FatorVencimento +
                            IntToStrZero(Round(ACBrTitulo.ValorDocumento * 100), 10) +
                            PadLeft(OnlyNumber(Cedente.Agencia), fpTamanhoAgencia, '0') +
                            PadLeft(OnlyNumber(Cedente.AgenciaDigito), 1, '0') +
                            PadLeft(OnlyNumber(Cedente.Convenio), 4, '0') +
                            PadLeft(OnlyNumber(ACBrTitulo.NossoNumero), fpTamanhoMaximoNossoNum, '0') +
                            '8';

          DigitoCodBarras := CalcularDigitoCodigoBarras(CodigoBarras);
     end;



     Result := PadLeft(OnlyNumber(IntToStr(fpNumero)), 3, '0') + '9' + DigitoCodBarras + Copy(CodigoBarras, 5, 39);

end;

function TACBrBancoAmazonia.MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String;
begin
     Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia + '-' +
               ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito + ' / ' +
               IntToStr(StrToIntDef(ACBrTitulo.ACBrBoleto.Cedente.Conta, 0)) + '-' +
               ACBrTitulo.ACBrBoleto.Cedente.ContaDigito;
end;

function TACBrBancoAmazonia.MontarCampoNossoNumero(const ACBrTitulo: TACBrTitulo): String;
var
     ANossoNumero: string;
     wTamConvenio: Integer;
begin
     ANossoNumero    := FormataNossoNumero(ACBrTitulo);
     wTamConvenio    := Length(ACBrBanco.ACBrBoleto.Cedente.Convenio);

     if (wTamConvenio = 4) and (ACBrTitulo.Carteira <> '001')  then
        Result := ANossoNumero + '.' + CalcularDigitoVerificador(ACBrTitulo)
     else
        Result := ANossoNumero;
end;

function TACBrBancoAmazonia.GerarRegistroHeader240(NumeroRemessa: Integer): String;
var
     ATipoInscricao: string;
     AMensagemReservada: String;
begin
     with ACBrBanco.ACBrBoleto.Cedente do
     begin
          case TipoInscricao of
               pFisica:
                    ATipoInscricao := '1';
               pJuridica:
                    ATipoInscricao := '2';
          end;

          if ACBrBanco.ACBrBoleto.Homologacao then
            AMensagemReservada := 'REMESSA-TESTE'
          else
            AMensagemReservada := 'REMESSA-PRODUCAO';

          { GERAR REGISTRO-HEADER DO ARQUIVO }

          Result := IntToStrZero(ACBrBanco.Numero, 3) +                            // 1 a 3 - Código do banco
               '0000' +                                                            // 4 a 7 - Lote de serviço
               '0' +                                                               // 8 - Tipo de registro - Registro header de arquivo
               PadRight('', 9, ' ') +                                              // 9 a 17 Uso exclusivo FEBRABAN/CNAB
               ATipoInscricao +                                                    // 18 - Tipo de inscrição do cedente
               PadLeft(OnlyNumber(CNPJCPF), 14, '0') +                             // 19 a 32 -Número de inscrição do cedente
               PadRight(OnlyNumber(Convenio), 20, ' ') +                           // 33 a 52 - Código do convênio no banco
               PadLeft(OnlyNumber(Agencia), 5, '0') +                              // 53 a 57 - Código da agência do cedente
               PadRight(AgenciaDigito, 1, '0') +                                   // 58 - Dígito da agência do cedente
               PadLeft(Conta, 12, '0') +                                           // 59 a 70 - Código da conta
               PadLeft(ContaDigito, 1, '0') +                                      // 71 - Digito da conta
               ' ' +                                                               // 72 - Uso Exclusivo BANCO
               PadRight(TiraAcentos(Nome), 30, ' ') +                              // 73 a 102 - Nome do cedente
               PadRight('BANCO DA AMAZONIA', 30, ' ') +                            // 103 a 132 - Nome do banco
               PadRight('', 10, ' ') +                                             // 133 a 142 - Uso exclusivo FEBRABAN/CNAB
               '1' +                                                               // 143 - Código de Remessa (1) / Retorno (2)
               FormatDateTime('ddmmyyyy', Now) +                                   // 144 a 151 - Data do de geração do arquivo
               FormatDateTime('hhmmss', Now) +                                     // 152 a 157 - Hora de geração do arquivo
               PadLeft(IntToStr(NumeroRemessa), 6, '0') +                          // 158 a 163 - Número seqüencial do arquivo
               '090' +                                                             // 164 a 166 - Número da versão do layout do arquivo
               PadRight('', 5, '0') +                                              // 167 a 171 - Densidade de gravação do arquivo (BPI)
               Space(20) +                                                         // 172 a 191 - Uso reservado do banco
               PadRight(AMensagemReservada, 20, ' ') +                             // 192 a 211 - Uso reservado da empresa
               PadRight('', 29, ' ');                                              // 212 a 240 - Uso Exclusivo FEBRABAN / CNAB

          { GERAR REGISTRO HEADER DO LOTE }

          Result := Result + #13#10 + IntToStrZero(ACBrBanco.Numero, 3) +          // 1 a 3 - Código do banco
               '0001' +                                                            // 4 a 7 - Lote de serviço
               '1' +                                                               // 8 - Tipo de registro - Registro header de arquivo
               'R' +                                                               // 9 - Tipo de operação: R (Remessa) ou T (Retorno)
               '01' +                                                              // 10 a 11 - Tipo de serviço: 01 (Cobrança)
               '  ' +                                                              // 12 a 13 - Forma de lançamento: preencher com ZEROS no caso de cobrança
               '047' +                                                             // 14 a 16 - Número da versão do layout do lote
               ' ' +                                                               // 17 - Uso exclusivo FEBRABAN/CNAB
               ATipoInscricao +                                                    // 18 - Tipo de inscrição do cedente
               PadLeft(OnlyNumber(CNPJCPF), 15, '0') +                             // 19 a 33 -Número de inscrição do cedente
               PadRight(OnlyNumber(Convenio), 20, ' ') +                           // 34 a 53 - Código do convênio no banco
               PadLeft(OnlyNumber(Agencia), 5, '0') +                              // 54 a 58 - Código da agência do cedente
               PadRight(AgenciaDigito, 1, '0') +                                   // 59 - Dígito da agência do cedente
               PadLeft(Conta, 12, '0') +                                           // 60 a 71 - Código da Conta
               PadRight(ContaDigito, 1, '0') +                                     // 72 - Digito da Conta
               ' ' +                                                               // 73 - Uso Exclusivo Caixa
               PadRight(TiraAcentos(Nome), 30, ' ') +                              // 74 a 103 - Nome do cedente
               PadRight('', 40, ' ') +                                             // 104 a 143 - Mensagem 1 para todos os boletos do lote
               PadRight('', 40, ' ') +                                             // 144 a 183 - Mensagem 2 para todos os boletos do lote
               PadLeft(IntToStr(NumeroRemessa), 8, '0') +                          // 184 a 191 - Número do arquivo
               FormatDateTime('ddmmyyyy', Now) +                                   // 192 a 199 - Data de geração do arquivo
               PadRight('', 8, '0') +                                              // 200 a 207 - Data do crédito - Só para arquivo retorno
               PadRight('', 33, ' ');                                              // 208 a 240 - Uso exclusivo FEBRABAN/CNAB
     end;
end;

function TACBrBancoAmazonia.GerarRegistroTransacao240(ACBrTitulo: TACBrTitulo): String;
var
     ATipoOcorrencia, ATipoBoleto: String;
     ADataMoraJuros, ADataDesconto: String;
     ANossoNumero, ATipoAceite: String;
     aAgencia, aConta, aDV: String;
     wTamConvenio, wTamNossoNum: Integer;
     wCarteira, wTipoDocumento: Integer;
     ACodBaixaDevolucao: String;
     ADiasBaixaDevolucao: String;
//     ACaracTitulo: Char;
begin
     with ACBrTitulo do
     begin
          ANossoNumero := FormataNossoNumero(ACBrTitulo);
          wTamConvenio := Length(ACBrBanco.ACBrBoleto.Cedente.Convenio);
          wTamNossoNum := CalcularTamMaximoNossoNumero(ACBrTitulo.Carteira, ACBrTitulo.NossoNumero);
          wCarteira := StrToIntDef(Carteira, 0);

          if (Integer(ACBrBoleto.Cedente.TipoDocumento) > 0) then
               wTipoDocumento := Integer(ACBrBoleto.Cedente.TipoDocumento)
          else wTipoDocumento := 1;

          if (((wCarteira = 11) or (wCarteira = 31) or (wCarteira = 51)) or
             ((wCarteira = 12) or (wCarteira = 15) or (wCarteira = 17))) and
             ((ACBrBoleto.Cedente.ResponEmissao <> tbCliEmite) and (StrToIntDef(NossoNumero, 0) = 0)) then
          begin
               ANossoNumero := StringOfChar('0', 20);
               aDV := ' ';
          end
          else
          begin
               ANossoNumero := FormataNossoNumero(ACBrTitulo);

               if (wTamNossoNum = 20) or (wTamConvenio = 7) or ((wTamConvenio = 6) and (wTamNossoNum = 17)) then
                    aDV := ''
               else
                    aDV := CalcularDigitoVerificador(ACBrTitulo);
          end;

          aAgencia := PadLeft(ACBrBoleto.Cedente.Agencia, 5, '0');
          aConta := PadLeft(ACBrBoleto.Cedente.Conta, 12, '0');

          { SEGMENTO P }

          { Pegando o Tipo de Ocorrencia }
          case OcorrenciaOriginal.Tipo of
               toRemessaBaixar:
                    ATipoOcorrencia := '02';
               toRemessaConcederAbatimento:
                    ATipoOcorrencia := '04';
               toRemessaCancelarAbatimento:
                    ATipoOcorrencia := '05';
               toRemessaAlterarVencimento:
                    ATipoOcorrencia := '06';
               toRemessaConcederDesconto:
                    ATipoOcorrencia := '07';
               toRemessaCancelarDesconto:
                    ATipoOcorrencia := '08';
               toRemessaProtestar:
                    ATipoOcorrencia := '09';
               toRemessaCancelarInstrucaoProtesto:
                    ATipoOcorrencia := '10';
               toRemessaAlterarNomeEnderecoSacado:
                    ATipoOcorrencia := '12';
               toRemessaDispensarJuros:
                    ATipoOcorrencia := '31';
          else
               ATipoOcorrencia := '01';
          end;

          { Pegando o tipo de EspecieDoc }
          if EspecieDoc = 'DM' then
               EspecieDoc := '02'
          else if EspecieDoc = 'RC' then
               EspecieDoc := '17'
          else if EspecieDoc = 'NP' then
               EspecieDoc := '12'
          else if EspecieDoc = 'NS' then
               EspecieDoc := '16'
          else if EspecieDoc = 'ND' then
               EspecieDoc := '19'
          else if EspecieDoc = 'DS' then
               EspecieDoc := '04';
//          else
//               EspecieDoc := EspecieDoc;

          { Pegando o Aceite do Titulo }
          case Aceite of
               atSim:
                    ATipoAceite := 'A';
               atNao:
                    ATipoAceite := 'N';
          else
               ATipoAceite := 'N';
          end;

          { Pegando Tipo de Boleto }
          case ACBrBoleto.Cedente.ResponEmissao of
               tbCliEmite:
                    ATipoBoleto := '2' + '2';
               tbBancoEmite:
                    ATipoBoleto := '1' + '1';
               tbBancoReemite:
                    ATipoBoleto := '4' + '1';
               tbBancoNaoReemite:
                    ATipoBoleto := '5' + '2';
          end;

//          ACaracTitulo := ' ';
//
//          case CaracTitulo of
//               tcSimples:
//                    ACaracTitulo := '1';
//               tcVinculada:
//                    ACaracTitulo := '2';
//               tcCaucionada:
//                    ACaracTitulo := '3';
//               tcDescontada:
//                    ACaracTitulo := '4';
//               tcVendor:
//                    ACaracTitulo := '5';
//          end;

          { Mora Juros }
          if (ValorMoraJuros > 0) and (DataMoraJuros > 0) then
               ADataMoraJuros := FormatDateTime('ddmmyyyy', DataMoraJuros)
          else
               ADataMoraJuros := PadRight('', 8, '0');

          { Descontos }
          if (ValorDesconto > 0) and (DataDesconto > 0) then
               ADataDesconto := FormatDateTime('ddmmyyyy', DataDesconto)
          else
               ADataDesconto := PadRight('', 8, '0');

          ANossoNumero := RemoveZerosEsquerda(ANossoNumero);

          {Código Baixa Devolução}
          if ((ATipoOcorrencia = '31') and (DataBaixa > Vencimento)) then
            ACodBaixaDevolucao := '3'
          else
            ACodBaixaDevolucao := IfThen(( ((DataProtesto = 0) or (DataProtesto <= Vencimento))
                               or (DataBaixa <> 0) and (DataBaixa > Vencimento)), '1', '2') ;

          {Dias Baixa Devolução}
          ADiasBaixaDevolucao := IfThen((DataBaixa <> 0) and (DataBaixa > Vencimento),
                 PadLeft(IntToStr(DaysBetween(DataBaixa, Vencimento)), 3, '0'), '000');

          { SEGMENTO P }
          Result := IntToStrZero(ACBrBanco.Numero, 3) +                                                                      // 1 a 3 - Código do banco
               '0001' +                                                                                                      // 4 a 7 - Lote de serviço
               '3' +                                                                                                         // 8 - Tipo do registro: Registro detalhe
               IntToStrZero((3 * ACBrBoleto.ListadeBoletos.IndexOf(ACBrTitulo)) + 1, 5) +                                    // 9 a 13 - Número seqüencial do registro no lote - Cada título tem 2 registros (P e Q)
               'P' +                                                                                                         // 14 - Código do segmento do registro detalhe
               ' ' +                                                                                                         // 15 - Uso exclusivo FEBRABAN/CNAB: Branco
               ATipoOcorrencia +                                                                                             // 16 a 17 - Código de movimento
               PadLeft(aAgencia, 5, ' ') +                                                                                   // 18 a 22 - Agência mantenedora da conta
               PadRight(ACBrBoleto.Cedente.AgenciaDigito, 1, '0') +                                                          // 23 -Dígito verificador da agência
               aConta +                                                                                                      // 24 a 35 - Número da conta corrente
               PadRight(ACBrBoleto.Cedente.ContaDigito, 1, '0') +                                                            // 36 - Dígito verificador da conta
               ' ' +                                                                                                         // 37 - Dígito verificador da agência / conta
               PadRight(ANossoNumero + aDV, 20, ' ') +                                                                       // 38 a 57 - Nosso número - identificação do título no banco
               PadRight(IntToStr(wCarteira), 1, '0') +                                                                       // 58 - Cobrança Simples
               '1' +                                                                                                         // 59 - Forma de cadastramento do título no banco: com cadastramento
               IntToStr(wTipoDocumento) +                                                                                    // 60 - Tipo de documento: Tradicional
               ATipoBoleto +                                                                                                 // 61 a 62 - Quem emite e quem distribui o boleto?
               PadRight(NumeroDocumento, 15, ' ') +                                                                          // 63 a 77 - Número que identifica o título na empresa [ Alterado conforme instruções da CSO Brasília ] {27-07-09}
               FormatDateTime('ddmmyyyy', Vencimento) +                                                                      // 78 a 85 - Data de vencimento do título
               IntToStrZero(Round(ValorDocumento * 100), 15) +                                                               // 86 a 100 - Valor nominal do título
               PadLeft(aAgencia, 5, ' ') +                                                                                   // 101 a 105 - Agência cobradora
               PadRight(ACBrBoleto.Cedente.AgenciaDigito, 1, '0') +                                                          // 106 Digito da agencia cobradora
               PadRight(EspecieDoc, 2) +                                                                                     // 107 a 108 - Espécie do documento
               ATipoAceite +                                                                                                 // 109 - Identificação de título Aceito / Não aceito
               FormatDateTime('ddmmyyyy', DataDocumento) +                                                                   // 110 a 117 - Data da emissão do documento
               IfThen(ValorMoraJuros > 0, '1', '3') +                                                                        // 118 - Código de juros de mora: Valor por dia
               ADataMoraJuros +                                                                                              // 119 a 126 - Data a partir da qual serão cobrados juros
               IfThen(ValorMoraJuros > 0, IntToStrZero(Round(ValorMoraJuros * 100), 15), PadRight('', 15, '0')) +            // 127 a 141 - Valor de juros de mora por dia
               IfThen(ValorDesconto > 0, IfThen(DataDesconto > 0, '1', '3'), '0') +                                          // 142 - Código de desconto: 1 - Valor fixo até a data informada 4-Desconto por dia de antecipacao 0 - Sem desconto
               IfThen(ValorDesconto > 0, IfThen(DataDesconto > 0, ADataDesconto, '00000000'), '00000000') +                  // 143 a 150 - Data do desconto
               IfThen(ValorDesconto > 0, IntToStrZero(Round(ValorDesconto * 100), 15), PadRight('', 15, '0')) +              // 151 a 165 - Valor do desconto por dia
               IntToStrZero(Round(ValorIOF * 100), 15) +                                                                     // 166 a 180 - Valor do IOF a ser recolhido
               IntToStrZero(Round(ValorAbatimento * 100), 15) +                                                              // 181 a 195 - Valor do abatimento
               PadRight(SeuNumero, 25, ' ') +                                                                                // 196 a 220 - Identificação do título na empresa
               IfThen((DataProtesto > 0) and (DataProtesto > Vencimento),
                      IfThen((DaySpan(Vencimento, DataProtesto) > 5), '1', '2'), '3') +                                      // 221 - Código de protesto: Protestar em XX dias corridos
               IfThen((DataProtesto > 0) and (DataProtesto > Vencimento),
                      PadLeft(IntToStr(DaysBetween(DataProtesto, Vencimento)), 2, '0'), '00') +                              // 222 a 223 - Prazo para protesto (em dias corridos)
               ACodBaixaDevolucao +                                                                                          // 224 - Código para baixa/devolução: Não baixar/não devolver
               ADiasBaixaDevolucao +                                                                                         // 225 a 227 - Prazo para baixa/devolução (em dias corridos)
               '09' +                                                                                                        // 228 a 229 - Código da moeda: Real
               StringOfChar('0', 10) +                                                                                       // 230 a 239 - Uso exclusivo FEBRABAN/CNAB
               ' ';                                                                                                          // 240 - Uso exclusivo FEBRABAN/CNAB

          { SEGMENTO Q }
          Result := Result + #13#10 + IntToStrZero(ACBrBanco.Numero, 3) +                                                    // 1 a 3 - Código do banco
               '0001' +                                                                                                      // 4 a 7 - Número do lote
               '3' +                                                                                                         // 8 - Tipo do registro: Registro detalhe
               IntToStrZero((3 * ACBrBoleto.ListadeBoletos.IndexOf(ACBrTitulo)) + 2, 5) +                                    // 9 a 13 - Número seqüencial do registro no lote - Cada título tem 2 registros (P e Q)
               'Q' +                                                                                                         // 14 - Código do segmento do registro detalhe
               ' ' +                                                                                                         // 15 - Uso exclusivo FEBRABAN/CNAB: Branco
               ATipoOcorrencia +                                                                                             // 16 - 17 - Tipo Ocorrencia
               IfThen(Sacado.Pessoa = pJuridica, '2', '1') +                                                                 // 18 - Tipo inscricao
               PadLeft(OnlyNumber(Sacado.CNPJCPF), 15, '0') +                                                                // 19 a 33 - Número de inscrição
               PadLeft(TiraAcentos(Sacado.NomeSacado), 40, ' ') +                                                            // 34 a 73 - Nome do Sacado
               PadLeft(TiraAcentos(Sacado.Logradouro + ' ' + Sacado.Numero + ' ' + Sacado.Complemento), 40, ' ') +           // 74 a 113 - Endereço do Sacado
               PadLeft(TiraAcentos(Sacado.Bairro), 15, ' ') +                                                                // 114 a 128 - Bairro do Sacado
               PadLeft(Sacado.CEP, 8, ' ') +                                                                                 // 129 a 136 - CEP do Sacado
               PadLeft(TiraAcentos(Sacado.Cidade), 15, ' ') +                                                                // 137 a 151 - Cidade do Sacado
               PadLeft(TiraAcentos(Sacado.UF), 2, ' ') +                                                                     // 152 a 153 - UF do Sacado
               PadLeft('', 1, '0') +                                                                                         // 154 - Tipo de inscricao do avalista
               PadLeft('', 15, '0') +                                                                                        // 155 a 169 - Inscricao do avalista
               PadLeft('', 40, ' ') +                                                                                        // 170 a 209 - Nome do Avalista
               PadRight('', 3, '0') +                                                                                        // 210 a 212 - Banco correspondente
               PadRight('', 20, ' ') +                                                                                       // 213 a 232 - Nosso número no banco correspondente
               PadRight('', 8, ' ');                                                                                         // 233 a 240 - Uso exclusivo FEBRABAN/CNAB

          { SEGMENTO R }
          Result := Result + #13#10 + IntToStrZero(ACBrBanco.Numero, 3) +                                                    // 1 - 3 Código do banco
               '0001' +                                                                                                      // 4 - 7 Número do lote
               '3' +                                                                                                         // 8 - 8 Tipo do registro: Registro detalhe
               IntToStrZero((3 * ACBrBoleto.ListadeBoletos.IndexOf(ACBrTitulo)) + 3, 5) +                                    // 9 - 13 Número seqüencial do registro no lote - Cada título tem 2 registros (P e Q)
               'R' +                                                                                                         // 14 - 14 Código do segmento do registro detalhe
               ' ' +                                                                                                         // 15 - 15 Uso exclusivo FEBRABAN/CNAB: Branco
               ATipoOcorrencia +                                                                                             // 16 - 17 Tipo Ocorrencia
               PadLeft('', 1, '0') +                                                                                         // 18 - Código do desconto 2
               PadLeft('', 8, '0') +                                                                                         // 19 a 26 - Data do desconto 2
               PadLeft('', 15, '0') +                                                                                        // 27 a 41 - Percentual concedido
               PadLeft('', 1, '0') +                                                                                         // 42 - Código do desconto 3
               PadLeft('', 8, '0') +                                                                                         // 43 a 50 - Data do desconto 3
               PadLeft('', 15, '0') +                                                                                        // 51 a 65 - Percentual concedido
               IfThen((PercentualMulta > 0), '2', '0') +                                                                     // 66 - 66 1-Cobrar Multa / 0-Não cobrar multa
               IfThen((PercentualMulta > 0), FormatDateTime('ddmmyyyy', DataMoraJuros), '00000000') +                        // 67 - 74 Se cobrar informe a data para iniciar a cobrança ou informe zeros se não cobrar
               IfThen(PercentualMulta > 0, IntToStrZero(Round(PercentualMulta * 100), 15), PadRight('', 15, '0')) +          // 75 - 89 Percentual de multa. Informar zeros se não cobrar
               PadLeft('', 10, ' ') +                                                                                        // 90 a 99 - Informação do Sacado
               PadLeft('', 40, ' ') +                                                                                        // 100 a 139 - Mensagem 3
               PadLeft('', 40, ' ') +                                                                                        // 140 a 179 - Mensagem 4
               PadLeft('', 20, ' ') +                                                                                        // 140 a 179 - Uso exclusivo FEBRABAN/CNAB: Branco
               PadRight('', 8, '0') +                                                                                        // 200 a 207 - Codigo de ocorrencia do sacado
               PadRight('', 3, '0') +                                                                                        // 208 a 210 - Codigo do banco na conta de débito
               PadRight('', 5, '0') +                                                                                        // 211 a 215 - Código da agencia na conta de débito
               PadRight('', 1, '0') +                                                                                        // 216 - Dígito verificador da agencia
               PadRight('',12, '0') +                                                                                        // 217 a 228 - Conta corrente de débito
               PadRight('', 1, '0') +                                                                                        // 229 - Dígito da conta corrente de débito
               PadRight('', 1, '0') +                                                                                        // 230 - Dígito da agencia/conta corrente de débito
               PadRight('', 1, '0') +                                                                                        // 231 - Aviso de débito automatico
               PadLeft('', 9, ' ');                                                                                          //  232 a 240 - Uso exclusivo FEBRABAN/CNAB: Branco
     end;
end;

function TACBrBancoAmazonia.GerarRegistroTrailler240(ARemessa: TStringList): String;
var
  aFEBRABAN : string;
begin
    if fpTamanhoMaximoNossoNum = 20 Then
     aFEBRABAN := IntToStrZero(3,6) //'Qtde de Contas p/ Conc';
    else
     aFEBRABAN := Space(6);
     { REGISTRO TRAILER DO LOTE }
     Result := IntToStrZero(ACBrBanco.Numero, 3) +                                  // 1 a 3 - Código do banco
          '0001' +                                                                  // 4 a 7 - Número do lote
          '5' +                                                                     // 8 - Tipo do registro: Registro trailer do lote
          Space(9) +                                                                // 9 a 17 - Uso exclusivo FEBRABAN/CNAB
          IntToStrZero((3 * (ARemessa.Count - 3)), 6) +                             // 18 a 23 - Quantidade de Registro da Remessa
          PadRight('', 6, '0') +                                                    // 24 a 29 - Quantidade títulos em cobrança
          PadRight('', 17, '0') +                                                   // 30 a 46 - Valor dos títulos em carteiras}
          PadRight('', 6, '0') +                                                    // 47 a 52 - Quantidade títulos em cobrança
          PadRight('', 17, '0') +                                                   // 53 a 69 - Valor dos títulos em carteiras}
          PadRight('', 6, '0') +                                                    // 70 a 75 - Quantidade títulos em cobrança
          PadRight('', 17, '0') +                                                   // 76 a 92 - Valor dos títulos em carteiras}
          PadRight('', 6, '0') +                                                    // 93 a 98 - Quantidade títulos em cobrança
          PadRight('', 17, '0') +                                                   // 99 a 115 - Valor dos títulos em carteiras}
          Space(8) +                                                                // 116 a 123 - Uso exclusivo FEBRABAN/CNAB}
          PadRight('', 117, ' ');                                                   // 124 a 240 - Uso exclusivo FEBRABAN/CNAB}

     { GERAR REGISTRO TRAILER DO ARQUIVO }
     Result := Result + #13#10 + IntToStrZero(ACBrBanco.Numero, 3) +                // 1 a 3 - Código do banco
          '9999' +                                                                  // 4 a 7 - Lote de serviço
          '9' +                                                                     // 8 - Tipo do registro: Registro trailer do arquivo
          Space(9) +                                                                // 9 a 17 - Uso exclusivo FEBRABAN/CNAB}
          '000001' +                                                                // 18 a 23 - Quantidade de lotes do arquivo}
          IntToStrZero(((ARemessa.Count - 3) * 3) + 4, 6) +                         // 24 a 29 - Quantidade de registros do arquivo, inclusive este registro que está sendo criado agora}
          aFEBRABAN +                                                               // 30 a 35 - Uso exclusivo FEBRABAN/CNAB}
          Space(205);                                                               // 36 a 240 - Uso exclusivo FEBRABAN/CNAB}
end;

procedure TACBrBancoAmazonia.GerarRegistroHeader400(NumeroRemessa: Integer; ARemessa: TStringList);
begin
     raise Exception.create(ACBrStr('CNAB 400 não implementado.'));
end;

procedure TACBrBancoAmazonia.GerarRegistroTransacao400(ACBrTitulo: TACBrTitulo; ARemessa: TStringList);
begin
     raise Exception.create(ACBrStr('CNAB 400 não implementado.'));
end;

procedure TACBrBancoAmazonia.GerarRegistroTrailler400(ARemessa: TStringList);
begin
     raise Exception.create(ACBrStr('CNAB 400 não implementado.'));
end;

procedure TACBrBancoAmazonia.LerRetorno240(ARetorno: TStringList);
var
     Titulo: TACBrTitulo;
     TempData, Linha, rCedente, rCNPJCPF: String;
     ContLinha: Integer;
     idxMotivo: Integer;
     rConvenioCedente: String;
begin
     // informação do Header
     // Verifica se o arquivo pertence ao banco
     if StrToIntDef(Copy(ARetorno.Strings[0], 1, 3), -1) <> Numero then
          raise Exception.create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno + 'não' + 'é um arquivo de retorno do ' + Nome));

     ACBrBanco.ACBrBoleto.DataArquivo := StringToDateTimeDef(Copy(ARetorno[0], 144, 2) + '/' +
                                         Copy(ARetorno[0], 146, 2) + '/' + Copy(ARetorno[0], 148, 4), 0, 'DD/MM/YYYY');

     ACBrBanco.ACBrBoleto.NumeroArquivo := StrToIntDef(Copy(ARetorno[0], 158, 6), 0);

     rCedente := Trim(Copy(ARetorno[0], 73, 30));
     rCNPJCPF := OnlyNumber(Copy(ARetorno[0], 19, 14));
     rConvenioCedente := Trim(RemoveZerosEsquerda(Copy(ARetorno[0], 33, 20)));

     with ACBrBanco.ACBrBoleto do
     begin
          if (not LeCedenteRetorno) and (rCNPJCPF <> OnlyNumber(Cedente.CNPJCPF)) then
               raise Exception.create(ACBrStr('CNPJ\CPF do arquivo inválido'));

          if LeCedenteRetorno then
          begin
               Cedente.Nome := rCedente;
               Cedente.CNPJCPF := rCNPJCPF;
               Cedente.Convenio := rConvenioCedente;
               Cedente.Agencia := Trim(Copy(ARetorno[0], 53, 5));
               Cedente.AgenciaDigito := Trim(Copy(ARetorno[0], 58, 1));
               Cedente.Conta := Trim(Copy(ARetorno[0], 59, 12));
               Cedente.ContaDigito := Trim(Copy(ARetorno[0], 71, 1));
          end;

          case StrToIntDef(Copy(ARetorno[0], 18, 1), 0) of
               01:
                    Cedente.TipoInscricao := pFisica;
          else
               Cedente.TipoInscricao := pJuridica;
          end;

          ACBrBanco.ACBrBoleto.ListadeBoletos.Clear;
     end;

     ACBrBanco.TamanhoMaximoNossoNum := 20;
     Linha := '';
     Titulo := nil;

     for ContLinha := 1 to ARetorno.Count - 2 do
     begin
          Linha := ARetorno[ContLinha];

          if Copy(Linha, 8, 1) <> '3' then // verifica se o registro (linha) é um registro detalhe (segmento J)
               Continue;

          if Copy(Linha, 14, 1) = 'T' then // se for segmento T cria um novo titulo
               Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

          if Assigned(Titulo) then
               with Titulo do
               begin
                    if Copy(Linha, 14, 1) = 'T' then
                    begin
                         SeuNumero := Copy(Linha, 106, 25);
                         NumeroDocumento := Copy(Linha, 59, 15);
                         Carteira := Copy(Linha, 58, 1);

                         TempData := Copy(Linha, 74, 2) + '/' + Copy(Linha, 76, 2) + '/' + Copy(Linha, 78, 4);
                         if TempData <> '00/00/0000' then
                              Vencimento := StringToDateTimeDef(TempData, 0, 'DD/MM/YYYY');

                         ValorDocumento := StrToFloatDef(Copy(Linha, 82, 15), 0) / 100;

                         if Length(ACBrBoleto.Cedente.Convenio) = 6 then
                           NossoNumero := Copy(Linha, 44, ACBrBanco.TamanhoMaximoNossoNum)
                         else
                           NossoNumero := Copy(Linha, 38, ACBrBanco.TamanhoMaximoNossoNum);
                         ValorDespesaCobranca := StrToFloatDef(Copy(Linha, 199, 15), 0) / 100;

                         OcorrenciaOriginal.Tipo := CodOcorrenciaToTipo(StrToIntDef(Copy(Linha, 16, 2), 0));

                         idxMotivo := 214;

                         while (idxMotivo < 223) do
                         begin
                              if (Trim(Copy(Linha, idxMotivo, 2)) <> '') then
                              begin
                                   MotivoRejeicaoComando.Add(Copy(Linha, idxMotivo, 2));
                                   DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo, StrToIntDef(Copy(Linha, idxMotivo, 2), 0)));
                              end;
                              Inc(idxMotivo, 2);
                         end;

                    end
                    else if Copy(Linha, 14, 1) = 'U' then // segmento U
                    begin
                         ValorIOF := StrToFloatDef(Copy(Linha, 63, 15), 0) / 100;
                         ValorAbatimento := StrToFloatDef(Copy(Linha, 48, 15), 0) / 100;
                         ValorDesconto := StrToFloatDef(Copy(Linha, 33, 15), 0) / 100;
                         ValorMoraJuros := StrToFloatDef(Copy(Linha, 18, 15), 0) / 100;
                         ValorOutrosCreditos := StrToFloatDef(Copy(Linha, 123, 15), 0) / 100;
                         ValorOutrasDespesas := StrToFloatDef(Copy(Linha, 108, 15), 0) / 100;
                         ValorRecebido := StrToFloatDef(Copy(Linha, 78, 15), 0) / 100;
                         TempData := Copy(Linha, 138, 2) + '/' + Copy(Linha, 140, 2) + '/' + Copy(Linha, 142, 4);
                         if TempData <> '00/00/0000' then
                           DataOcorrencia := StringToDateTimeDef(TempData, 0, 'DD/MM/YYYY');
                         TempData := Copy(Linha, 146, 2) + '/' + Copy(Linha, 148, 2) + '/' + Copy(Linha, 150, 4);
                         if TempData <> '00/00/0000' then
                           DataCredito := StringToDateTimeDef(TempData, 0, 'DD/MM/YYYY');
                    end;
               end;
     end;

     ACBrBanco.TamanhoMaximoNossoNum := 10;
end;

function TACBrBancoAmazonia.TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia): String;
begin
     Result := '';

     if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
     begin
          case TipoOcorrencia of
               toRetornoTransferenciaCarteiraEntrada:
                    Result := '04';
               toRetornoTransferenciaCarteiraBaixa:
                    Result := '05';
               toRetornoBaixaAutomatica:
                    Result := '09';
               toRetornoBaixadoFrancoPagamento:
                    Result := '15';
               toRetornoLiquidadoSemRegistro:
                    Result := '17';
               toRetornoRecebimentoInstrucaoSustarProtesto:
                    Result := '20';
               toRetornoRetiradoDeCartorio:
                    Result := '24';
               toRetornoBaixaPorProtesto:
                    Result := '25';
               toRetornoInstrucaoRejeitada:
                    Result := '26';
               toRetornoAlteracaoUsoCedente:
                    Result := '27';
               toRetornoDebitoTarifas:
                    Result := '28';
               toRetornoOcorrenciasDoSacado:
                    Result := '29';
               toRetornoAlteracaoDadosRejeitados:
                    Result := '30';
               toRetornoChequePendenteCompensacao:
                    Result := '50';
          end;
     end
     else
     begin
          case TipoOcorrencia of
               toRetornoLiquidadoSemRegistro:
                    Result := '05';
               toRetornoLiquidadoPorConta:
                    Result := '08';
               toRetornoLiquidadoSaldoRestante:
                    Result := '08';
               toRetornoBaixaSolicitada:
                    Result := '10';
               toRetornoLiquidadoEmCartorio:
                    Result := '15';
               toRetornoConfirmacaoAlteracaoJurosMora:
                    Result := '16';
               toRetornoDebitoEmConta:
                    Result := '20';
               toRetornoNomeSacadoAlterado:
                    Result := '21';
               toRetornoEnderecoSacadoAlterado:
                    Result := '22';
               toRetornoProtestoSustado:
                    Result := '24';
               toRetornoJurosDispensados:
                    Result := '25';
               toRetornoManutencaoTituloVencido:
                    Result := '28';
               toRetornoDescontoConcedido:
                    Result := '31';
               toRetornoDescontoCancelado:
                    Result := '32';
               toRetornoDescontoRetificado:
                    Result := '33';
               toRetornoAlterarDataDesconto:
                    Result := '34';
               toRetornoRecebimentoInstrucaoAlterarJuros:
                    Result := '35';
               toRetornoRecebimentoInstrucaoDispensarJuros:
                    Result := '36';
               toRetornoDispensarIndexador:
                    Result := '37';
               toRetornoDispensarPrazoLimiteRecebimento:
                    Result := '38';
               toRetornoAlterarPrazoLimiteRecebimento:
                    Result := '39';
               toRetornoChequePendenteCompensacao:
                    Result := '46';
               toRetornoTipoCobrancaAlterado:
                    Result := '72';
               toRetornoDespesasProtesto:
                    Result := '96';
               toRetornoDespesasSustacaoProtesto:
                    Result := '97';
               toRetornoDebitoCustasAntecipadas:
                    Result := '98';
          end;
     end;

     if (Result <> '') then
          Exit;

     case TipoOcorrencia of
          toRetornoRegistroConfirmado:
               Result := '02';
          toRetornoRegistroRecusado:
               Result := '03';
          toRetornoLiquidado:
               Result := '06';
          toRetornoTituloEmSer:
               Result := '11';
          toRetornoAbatimentoConcedido:
               Result := '12';
          toRetornoAbatimentoCancelado:
               Result := '13';
          toRetornoVencimentoAlterado:
               Result := '14';
          toRetornoRecebimentoInstrucaoProtestar:
               Result := '19';
          toRetornoEntradaEmCartorio:
               Result := '23';
          toRetornoChequeDevolvido:
               Result := '44';
     else
          Result := '02';
     end;
end;

function TACBrBancoAmazonia.TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String;
var
     CodOcorrencia: Integer;
begin

     Result := '';
     CodOcorrencia := StrToIntDef(TipoOcorrenciaToCod(TipoOcorrencia), 0);

     if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
     begin
          case CodOcorrencia of
               02:
                    Result := '02 – Entrada confirmada';
               03:
                    Result := '03 – Entrada Rejeitada';
               04:
                    Result := '04 – Transferência de Carteira/Entrada';
               05:
                    Result := '05 – Transferência de Carteira/Baixa';
               06:
                    Result := '06 – Liquidação';
               09:
                    Result := '09 – Baixa';
               11:
                    Result := '11 – Títulos em Carteira (em ser)';
               12:
                    Result := '12 – Confirmação Recebimento Instrução de Abatimento';
               13:
                    Result := '13 – Confirmação Recebimento Instrução de Cancelamento Abatimento';
               14:
                    Result := '14 – Confirmação Recebimento Instrução Alteração de Vencimento';
               15:
                    Result := '15 – Franco de Pagamento';
               17:
                    Result := '17 – Liquidação Após Baixa ou Liquidação Título Não Registrado';
               19:
                    Result := '19 – Confirmação Recebimento Instrução de Protesto';
               20:
                    Result := '20 – Confirmação Recebimento Instrução de Sustação/Cancelamento de Protesto';
               23:
                    Result := '23 – Remessa a Cartório';
               24:
                    Result := '24 – Retirada de Cartório e Manutenção em Carteira';
               25:
                    Result := '25 – Protestado e Baixado';
               26:
                    Result := '26 – Instrução Rejeitada';
               27:
                    Result := '27 – Confirmação do Pedido de Alteração de Outros Dados';
               28:
                    Result := '28 – Débito de Tarifas/Custas';
               29:
                    Result := '29 – Ocorrências do Sacado';
               30:
                    Result := '30 – Alteração de Dados Rejeitada';
               44:
                    Result := '44 – Título pago com cheque devolvido';
               50:
                    Result := '50 – Título pago com cheque pendente de compensação'
          end;
     end
     else
     begin
          case CodOcorrencia of
               02:
                    Result := '02-Confirmação de Entrada de Título';
               03:
                    Result := '03-Comando recusado';
               05:
                    Result := '05-Liquidado sem registro';
               06:
                    Result := '06-Liquidação Normal';
               07:
                    Result := '07-Liquidação por Conta';
               08:
                    Result := '08-Liquidação por Saldo';
               09:
                    Result := '09-Baixa de Título';
               10:
                    Result := '10-Baixa Solicitada';
               11:
                    Result := '11-Titulos em Ser';
               12:
                    Result := '12-Abatimento Concedido';
               13:
                    Result := '13-Abatimento Cancelado';
               14:
                    Result := '14-Alteração de Vencimento do Titulo';
               15:
                    Result := '15-Liquidação em Cartório';
               16:
                    Result := '16-Confirmação de alteração de juros de mora';
               19:
                    Result := '19-Confirmação de recebimento de instruções para protesto';
               20:
                    Result := '20-Débito em Conta';
               21:
                    Result := '21-Alteração do Nome do Sacado';
               22:
                    Result := '22-Alteração do Endereço do Sacado';
               23:
                    Result := '23-Indicação de encaminhamento a cartório';
               24:
                    Result := '24-Sustar Protesto';
               25:
                    Result := '25-Dispensar Juros';
               28:
                    Result := '28-Manutenção de titulo vencido';
               31:
                    Result := '31-Conceder desconto';
               32:
                    Result := '32-Não conceder desconto';
               33:
                    Result := '33-Retificar desconto';
               34:
                    Result := '34-Alterar data para desconto';
               35:
                    Result := '35-Cobrar multa';
               36:
                    Result := '36-Dispensar multa';
               37:
                    Result := '37-Dispensar indexador';
               38:
                    Result := '38-Dispensar prazo limite para recebimento';
               39:
                    Result := '39-Alterar prazo limite para recebimento';
               44:
                    Result := '44-Título pago com cheque devolvido';
               46:
                    Result := '46-Título pago com cheque, aguardando compensação';
               72:
                    Result := '72-Alteração de tipo de cobrança';
               96:
                    Result := '96-Despesas de Protesto';
               97:
                    Result := '97-Despesas de Sustação de Protesto';
               98:
                    Result := '98-Débito de Custas Antecipadas';
          end;
     end;

     Result := ACBrSTr(Result);
end;

function TACBrBancoAmazonia.CodOcorrenciaToTipo(const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
     Result := toTipoOcorrenciaNenhum;

     if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
     begin
          case CodOcorrencia of
               04:
                    Result := toRetornoTransferenciaCarteiraEntrada;
               05:
                    Result := toRetornoTransferenciaCarteiraBaixa;
               09:
                    Result := toRetornoBaixaAutomatica;
               15:
                    Result := toRetornoBaixadoFrancoPagamento;
               17:
                    Result := toRetornoLiquidadoSemRegistro;
               20:
                    Result := toRetornoRecebimentoInstrucaoSustarProtesto;
               24:
                    Result := toRetornoRetiradoDeCartorio;
               25:
                    Result := toRetornoBaixaPorProtesto;
               26:
                    Result := toRetornoInstrucaoRejeitada;
               27:
                    Result := toRetornoAlteracaoUsoCedente;
               28:
                    Result := toRetornoDebitoTarifas;
               29:
                    Result := toRetornoOcorrenciasDoSacado;
               30:
                    Result := toRetornoAlteracaoDadosRejeitados;
               50:
                    Result := toRetornoChequePendenteCompensacao;
          end;
     end
     else
     begin
          case CodOcorrencia of
               05:
                    Result := toRetornoLiquidadoSemRegistro;
               07:
                    Result := toRetornoLiquidadoPorConta;
               08:
                    Result := toRetornoLiquidadoSaldoRestante;
               10:
                    Result := toRetornoBaixaSolicitada;
               15:
                    Result := toRetornoLiquidadoEmCartorio;
               16:
                    Result := toRetornoConfirmacaoAlteracaoJurosMora;
               20:
                    Result := toRetornoDebitoEmConta;
               21:
                    Result := toRetornoNomeSacadoAlterado;
               22:
                    Result := toRetornoEnderecoSacadoAlterado;
               24:
                    Result := toRetornoProtestoSustado;
               25:
                    Result := toRetornoJurosDispensados;
               28:
                    Result := toRetornoManutencaoTituloVencido;
               31:
                    Result := toRetornoDescontoConcedido;
               32:
                    Result := toRetornoDescontoCancelado;
               33:
                    Result := toRetornoDescontoRetificado;
               34:
                    Result := toRetornoAlterarDataDesconto;
               35:
                    Result := toRetornoRecebimentoInstrucaoAlterarJuros;
               36:
                    Result := toRetornoRecebimentoInstrucaoDispensarJuros;
               37:
                    Result := toRetornoDispensarIndexador;
               38:
                    Result := toRetornoDispensarPrazoLimiteRecebimento;
               39:
                    Result := toRetornoAlterarPrazoLimiteRecebimento;
               46:
                    Result := toRetornoChequePendenteCompensacao;
               72:
                    Result := toRetornoTipoCobrancaAlterado;
               96:
                    Result := toRetornoDespesasProtesto;
               97:
                    Result := toRetornoProtestoSustado;
               98:
                    Result := toRetornoDebitoCustasAntecipadas;
          end;
     end;

     if (Result <> toTipoOcorrenciaNenhum) then
          Exit;

     case CodOcorrencia of
          02:
               Result := toRetornoRegistroConfirmado;
          03:
               Result := toRetornoRegistroRecusado;
          06:
               Result := toRetornoLiquidado;
          11:
               Result := toRetornoTituloEmSer;
          12:
               Result := toRetornoAbatimentoConcedido;
          13:
               Result := toRetornoAbatimentoCancelado;
          14:
               Result := toRetornoVencimentoAlterado;
          19:
               Result := toRetornoRecebimentoInstrucaoProtestar;
          23:
               Result := toRetornoEntradaEmCartorio;
          44:
               Result := toRetornoChequeDevolvido;
     else
          Result := toRetornoOutrasOcorrencias;
     end;
end;

function TACBrBancoAmazonia.CodMotivoRejeicaoToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: Integer): String;
begin
     case TipoOcorrencia of
          toRetornoComandoRecusado: // 03 (Recusado)
               case CodMotivo of
                    01:
                         Result := '01-Identificação inválida';
                    02:
                         Result := '02-Variação da carteira inválida';
                    03:
                         Result := '03-Valor dos juros por um dia inválido';
                    04:
                         Result := '04-Valor do desconto inválido';
                    05:
                         Result := '05-Espécie de título inválida para carteira';
                    06:
                         Result := '06-Espécie de valor variável inválido';
                    07:
                         Result := '07-Prefixo da agência usuária inválido';
                    08:
                         Result := '08-Valor do título/apólice inválido';
                    09:
                         Result := '09-Data de vencimento inválida';
                    10:
                         Result := '10-Fora do prazo';
                    11:
                         Result := '11-Inexistência de margem para desconto';
                    12:
                         Result := '12-O Banco não tem agência na praça do sacado';
                    13:
                         Result := '13-Razões cadastrais';
                    14:
                         Result := '14-Sacado interligado com o sacador';
                    15:
                         Result := '15-Título sacado contra orgão do Poder Público';
                    16:
                         Result := '16-Título preenchido de forma irregular';
                    17:
                         Result := '17-Título rasurado';
                    18:
                         Result := '18-Endereço do sacado não localizado ou incompleto';
                    19:
                         Result := '19-Código do cedente inválido';
                    20:
                         Result := '20-Nome/endereco do cliente não informado /ECT/';
                    21:
                         Result := '21-Carteira inválida';
                    22:
                         Result := '22-Quantidade de valor variável inválida';
                    23:
                         Result := '23-Faixa nosso número excedida';
                    24:
                         Result := '24-Valor do abatimento inválido';
                    25:
                         Result := '25-Novo número do título dado pelo cedente inválido';
                    26:
                         Result := '26-Valor do IOF de seguro inválido';
                    27:
                         Result := '27-Nome do sacado/cedente inválido ou não informado';
                    28:
                         Result := '28-Data do novo vencimento inválida';
                    29:
                         Result := '29-Endereco não informado';
                    30:
                         Result := '30-Registro de título já liquidado';
                    31:
                         Result := '31-Numero do bordero inválido';
                    32:
                         Result := '32-Nome da pessoa autorizada inválido';
                    33:
                         Result := '33-Nosso número já existente';
                    34:
                         Result := '34-Numero da prestação do contrato inválido';
                    35:
                         Result := '35-Percentual de desconto inválido';
                    36:
                         Result := '36-Dias para fichamento de protesto inválido';
                    37:
                         Result := '37-Data de emissão do título inválida';
                    38:
                         Result := '38-Data do vencimento anterior a data da emissão do título';
                    39:
                         Result := '39-Comando de alteração indevido para a carteira';
                    40:
                         Result := '40-Tipo de moeda inválido';
                    41:
                         Result := '41-Abatimento não permitido';
                    42:
                         Result := '42-CEP do sacado inválido /ECT/';
                    43:
                         Result := '43-Codigo de unidade variavel incompativel com a data emissão do título';
                    44:
                         Result := '44-Dados para debito ao sacado inválidos';
                    45:
                         Result := '45-Carteira';
                    46:
                         Result := '46-Convenio encerrado';
                    47:
                         Result := '47-Título tem valor diverso do informado';
                    48:
                         Result := '48-Motivo de baixa inválido para a carteira';
                    49:
                         Result := '49-Abatimento a cancelar não consta do título';
                    50:
                         Result := '50-Comando incompativel com a carteira';
                    51:
                         Result := '51-Codigo do convenente inválido';
                    52:
                         Result := '52-Abatimento igual ou maior que o valor do título';
                    53:
                         Result := '53-Título já se encontra situação pretendida';
                    54:
                         Result := '54-Título fora do prazo admitido para a conta 1';
                    55:
                         Result := '55-Novo vencimento fora dos limites da carteira';
                    56:
                         Result := '56-Título não pertence ao convenente';
                    57:
                         Result := '57-Variação incompativel com a carteira';
                    58:
                         Result := '58-Impossivel a transferencia para a carteira indicada';
                    59:
                         Result := '59-Título vencido em transferencia para a carteira 51';
                    60:
                         Result := '60-Título com prazo superior a 179 dias em transferencia para carteira 51';
                    61:
                         Result := '61-Título já foi fichado para protesto';
                    62:
                         Result := '62-Alteração da situação de debito inválida para o codigo de responsabilidade';
                    63:
                         Result := '63-DV do nosso número inválido';
                    64:
                         Result := '64-Título não passivel de debito/baixa - situação anormal';
                    65:
                         Result := '65-Título com ordem de não protestar-não pode ser encaminhado a cartorio';
                    66:
                         Result := '66-Número do documento do sacado (CNPJ/CPF) inválido';
                    67:
                         Result := '66-Título/carne rejeitado';
                    68:
                         Result := '68-Código/Data/Percentual de multa inválido';
                    69:
                         Result := '69-Valor/Percentual de Juros Inválido';
                    70:
                         Result := '70-Título já se encontra isento de juros';
                    71:
                         Result := '71-Código de Juros Inválido';
                    72:
                         Result := '72-Prefixo da Ag. cobradora inválido';
                    73:
                         Result := '73–Numero do controle do participante inválido';
                    74:
                         Result := '74–Cliente não cadastrado no CIOPE (Desconto/Vendor)';
                    75:
                         Result := '75–Qtde. de dias do prazo limite p/ recebimento de título vencido inválido';
                    76:
                         Result := '76–Titulo excluído automaticamente por decurso deprazo CIOPE (Desconto/Vendor)';
                    77:
                         Result := '77–Titulo vencido transferido para a conta 1 – Carteira vinculada';
                    80:
                         Result := '80-Nosso número inválido';
                    81:
                         Result := '81-Data para concessão do desconto inválida';
                    82:
                         Result := '82-CEP do sacado inválido';
                    83:
                         Result := '83-Carteira/variação não localizada no cedente';
                    84:
                         Result := '84-Título não localizado na existencia';
                    99:
                         Result := '99-Outros motivos';
               end;
          toRetornoLiquidadoSemRegistro, // 05-Liquidado sem registro (carteira 17-tipo4)
          toRetornoLiquidado, // 06-Liquidação Normal
          toRetornoLiquidadoPorConta, // 07-Liquidação por Conta
          toRetornoLiquidadoEmCartorio, // 15-Liquidação em Cartório
          toRetornoTituloPagoEmCheque, // 46–Título pago com cheque, aguardando compensação
          toRetornoLiquidadoAposBaixaOuNaoRegistro: // 17-Liquidação Após Baixa ou Liquidação de Título Não Registrado
               case CodMotivo of
                    01:
                         Result := '01-Liquidação normal';
                    02:
                         Result := '02-Liquidação parcial';
                    03:
                         Result := '03-Liquidação por saldo';
                    04:
                         Result := '04-Liquidação com cheque a compensar';
                    05:
                         Result := '05-Liquidação de título sem registro (carteira 7 tipo 4)';
                    07:
                         Result := '07-Liquidação na apresentação';
                    09:
                         Result := '09-Liquidação em cartório';
               end;
          toRetornoRegistroConfirmado: // 02 (Entrada)
               case CodMotivo of
                    00:
                         Result := '00-Por meio magnético';
                    11:
                         Result := '11-Por via convencional';
                    16:
                         Result := '16-Por alteração do código do cedente';
                    17:
                         Result := '17-Por alteração da variação';
                    18:
                         Result := '18-Por alteração de carteira';
               end;
          toRetornoBaixado, toRetornoBaixaSolicitada, toRetornoDebitoEmConta: // 09, 10 ou 20 (Baixa)
               case CodMotivo of
                    00:
                         Result := '00-Solicitada pelo cliente';
                    14:
                         Result := '14-Protestado';
                    15:
                         case ACBrBanco.ACBrBoleto.LayoutRemessa of
                              c240:
                                   Result := '15-Título Excluído';
                              c400:
                                   Result := '15-Protestado';
                         end;
                    18:
                         Result := '18-Por alteração de carteira';
                    19:
                         Result := '19-Débito automático';
                    31:
                         Result := '31-Liquidado anteriormente';
                    32:
                         Result := '32-Habilitado em processo';
                    33:
                         Result := '33-Incobrável por nosso intermédio';
                    34:
                         Result := '34-Transferido para créditos em liquidação';
                    46:
                         Result := '46-Por alteração da variação';
                    47:
                         Result := '47-Por alteração da variação';
                    51:
                         Result := '51-Acerto';
                    90:
                         Result := '90-Baixa automática';
               end;
          toRetornoDebitoTarifas: // 28 - Débito de Tarifas/Custas (Febraban 240 posições, v08.9 de 15/04/2014)
               case CodMotivo of
                    01:
                         Result := '01-Tarifa de Extrato de Posição';
                    02:
                         Result := '02-Tarifa de Manutenção de Título Vencido';
                    03:
                         Result := '03-Tarifa de Sustação';
                    04:
                         Result := '04-Tarifa de Protesto';
                    05:
                         Result := '05-Tarifa de Outras Instruções';
                    06:
                         Result := '06-Tarifa de Outras Ocorrências';
                    07:
                         Result := '07-Tarifa de Envio de Duplicata ao Sacado';
                    08:
                         Result := '08-Custas de Protesto';
                    09:
                         Result := '09-Custas de Sustação de Protesto';
                    10:
                         Result := '10-Custas de Cartório Distribuidor';
                    11:
                         Result := '11-Custas de Edital';
                    12:
                         Result := '12-Tarifa Sobre Devolução de Título Vencido';
                    13:
                         Result := '13-Tarifa Sobre Registro Cobrada na Baixa/Liquidação';
                    14:
                         Result := '14-Tarifa Sobre Reapresentação Automática';
                    15:
                         Result := '15-Tarifa Sobre Rateio de Crédito';
                    16:
                         Result := '16-Tarifa Sobre Informações Via Fax';
                    17:
                         Result := '17-Tarifa Sobre Prorrogação de Vencimento';
                    18:
                         Result := '18-Tarifa Sobre Alteração de Abatimento/Desconto';
                    19:
                         Result := '19-Tarifa Sobre Arquivo mensal (Em Ser)';
                    20:
                         Result := '20-Tarifa Sobre Emissão de Bloqueto Pré-Emitido pelo Banco';
               end;
          toRetornoTipoCobrancaAlterado:
               case CodMotivo of
                    00:
                         Result := '00-Transferência de título de cobrança simples para descontada ou vice-versa';
                    52:
                         Result := '52-Reembolso de título vendor ou descontado';
               end;
     end;

     Result := ACBrSTr(Result);
end;

procedure TACBrBancoAmazonia.LerRetorno400(ARetorno: TStringList);
var
     TamConvenioMaior6: Boolean;
begin
     TamConvenioMaior6 := Length(Trim(ACBrBanco.ACBrBoleto.Cedente.Convenio)) > 6;
     if TamConvenioMaior6 then
          LerRetorno400Pos7(ARetorno)
     else
          LerRetorno400Pos6(ARetorno);
end;

procedure TACBrBancoAmazonia.LerRetorno400Pos6(ARetorno: TStringList);
var
     Titulo: TACBrTitulo;
     ContLinha, CodOcorrencia, CodMotivo, MotivoLinha: Integer;
     rAgencia, rDigitoAgencia, rConta: String;
     rDigitoConta, rCodigoCedente: String;
     Linha, rCedente: String;
     rConvenioCedente: String;
begin
     fpTamanhoMaximoNossoNum := 11;

     if StrToIntDef(Copy(ARetorno.Strings[0], 77, 3), -1) <> Numero then
          raise Exception.create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno + 'não é um arquivo de retorno do ' + Nome));

     rCedente := Trim(Copy(ARetorno[0], 47, 30));
     rAgencia := Trim(Copy(ARetorno[0], 27, 4));
     rDigitoAgencia := Copy(ARetorno[0], 31, 1);
     rConta := Trim(Copy(ARetorno[0], 32, 8));
     rDigitoConta := Copy(ARetorno[0], 40, 1);

     rCodigoCedente := Copy(ARetorno[0], 41, 6);
     rConvenioCedente := Copy(ARetorno[0], 41, 6);

     ACBrBanco.ACBrBoleto.NumeroArquivo := StrToIntDef(Copy(ARetorno[0], 101, 7), 0);

     ACBrBanco.ACBrBoleto.DataArquivo := StringToDateTimeDef(Copy(ARetorno[0], 95, 2) + '/' +
                                         Copy(ARetorno[0], 97, 2) + '/' + Copy(ARetorno[0], 99, 2), 0, 'DD/MM/YY');

     ValidarDadosRetorno(rAgencia, rConta);
     with ACBrBanco.ACBrBoleto do
     begin
        if LeCedenteRetorno then
        begin
                 Cedente.Nome := rCedente;
                 Cedente.Agencia := rAgencia;
                 Cedente.AgenciaDigito := rDigitoAgencia;
                 Cedente.Conta := rConta;
                 Cedente.ContaDigito := rDigitoConta;
                 Cedente.CodigoCedente := rCodigoCedente;
                 Cedente.Convenio := rConvenioCedente;
        end;

        ACBrBanco.ACBrBoleto.ListadeBoletos.Clear;
     end;

     ACBrBanco.TamanhoMaximoNossoNum := fpTamanhoMaximoNossoNum;

     for ContLinha := 1 to ARetorno.Count - 2 do
     begin
          Linha := ARetorno[ContLinha];

          if (Copy(Linha, 1, 1) <> '1') then
               Continue;

          Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

          with Titulo do
          begin
               SeuNumero := Copy(Linha, 38, 25);
               NumeroDocumento := Copy(Linha, 117, 10);

               CodOcorrencia := StrToIntDef(Copy(Linha, 109, 2), 0);
               OcorrenciaOriginal.Tipo := CodOcorrenciaToTipo(CodOcorrencia);

               if ((CodOcorrencia >= 5) and (CodOcorrencia <= 8)) or (CodOcorrencia = 15) or (CodOcorrencia = 46) then
               begin
                    CodigoLiquidacao := IntToStr(CodOcorrencia);
                    CodigoLiquidacaoDescricao := TipoOcorrenciaToDescricao(OcorrenciaOriginal.Tipo);
               end;

               if (CodOcorrencia >= 2) and (CodOcorrencia <= 10) then
               begin
                    MotivoLinha := 81;
                    CodMotivo := StrToInt(Copy(Linha, MotivoLinha, 2));
                    MotivoRejeicaoComando.Add(Copy(Linha, MotivoLinha, 2));
                    DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo, CodMotivo));
               end;

               DataOcorrencia := StringToDateTimeDef(Copy(Linha, 111, 2) + '/' +
                                 Copy(Linha, 113, 2) + '/' + Copy(Linha, 115, 2), 0, 'DD/MM/YY');

               Vencimento := StringToDateTimeDef(Copy(Linha, 147, 2) + '/' +
                             Copy(Linha, 149, 2) + '/' + Copy(Linha, 151, 2), 0, 'DD/MM/YY');

               ValorDocumento := StrToFloatDef(Copy(Linha, 153, 13), 0) / 100;
               ValorIOF := StrToFloatDef(Copy(Linha, 215, 13), 0) / 100;
               ValorAbatimento := StrToFloatDef(Copy(Linha, 228, 13), 0) / 100;
               ValorDesconto := StrToFloatDef(Copy(Linha, 241, 13), 0) / 100;
               ValorRecebido := StrToFloatDef(Copy(Linha, 254, 13), 0) / 100;
               ValorMoraJuros := StrToFloatDef(Copy(Linha, 267, 13), 0) / 100;
               ValorOutrosCreditos := StrToFloatDef(Copy(Linha, 280, 13), 0) / 100;
               Carteira := Copy(Linha, 107, 2);
               NossoNumero := Copy(Linha, 63, 11);
               ValorDespesaCobranca := StrToFloatDef(Copy(Linha, 182, 07), 0) / 100;
               ValorOutrasDespesas := StrToFloatDef(Copy(Linha, 189, 13), 0) / 100;

               if StrToIntDef(Copy(Linha, 176, 6), 0) <> 0 then
                    DataCredito := StringToDateTimeDef(Copy(Linha, 176, 2) + '/' +
                                   Copy(Linha, 178, 2) + '/' + Copy(Linha, 180, 2), 0, 'DD/MM/YY');
          end;
     end;

     fpTamanhoMaximoNossoNum := 10;
end;

procedure TACBrBancoAmazonia.LerRetorno400Pos7(ARetorno: TStringList);
var
     Titulo: TACBrTitulo;
     ContLinha, CodOcorrencia, CodMotivo: Integer;
     rAgencia, rDigitoAgencia, rConta: String;
     rDigitoConta, rCodigoCedente: String;
     Linha, rCedente: String;
     rConvenioCedente: String;
begin
     fpTamanhoMaximoNossoNum := 20;

     if StrToIntDef(Copy(ARetorno.Strings[0], 77, 3), -1) <> Numero then
          raise Exception.create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno + 'não é um arquivo de retorno do ' + Nome));

     rCedente := Trim(Copy(ARetorno[0], 47, 30));
     rAgencia := Trim(Copy(ARetorno[0], 27, 4));
     rDigitoAgencia := Copy(ARetorno[0], 31, 1);
     rConta := Trim(Copy(ARetorno[0], 32, 8));
     rDigitoConta := Copy(ARetorno[0], 40, 1);

     rCodigoCedente := Copy(ARetorno[0], 150, 7);
     rConvenioCedente := Copy(ARetorno[0], 150, 7);

     ACBrBanco.ACBrBoleto.NumeroArquivo := StrToIntDef(Copy(ARetorno[0], 101, 7), 0);

     ACBrBanco.ACBrBoleto.DataArquivo := StringToDateTimeDef(Copy(ARetorno[0], 95, 2) +
                                         '/' + Copy(ARetorno[0], 97, 2) + '/' + Copy(ARetorno[0], 99, 2), 0, 'DD/MM/YY');

     ValidarDadosRetorno(rAgencia, rConta);
     with ACBrBanco.ACBrBoleto do
     begin
       if LeCedenteRetorno then
       begin
            Cedente.Nome := rCedente;
            Cedente.Agencia := rAgencia;
            Cedente.AgenciaDigito := rDigitoAgencia;
            Cedente.Conta := rConta;
            Cedente.ContaDigito := rDigitoConta;
            Cedente.CodigoCedente := rCodigoCedente;
            Cedente.Convenio := rConvenioCedente;
       end;

       ACBrBanco.ACBrBoleto.ListadeBoletos.Clear;
     end;

     ACBrBanco.TamanhoMaximoNossoNum := 20;

     for ContLinha := 1 to ARetorno.Count - 2 do
     begin
          Linha := ARetorno[ContLinha];

          if (Copy(Linha, 1, 1) <> '7') and (Copy(Linha, 1, 1) <> '1') then
               Continue;

          Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

          with Titulo do
          begin
               SeuNumero := Copy(Linha, 39, 25);
               NumeroDocumento := Copy(Linha, 117, 10);
               CodOcorrencia := StrToIntDef(Copy(Linha, 109, 2), 0);
               OcorrenciaOriginal.Tipo := CodOcorrenciaToTipo(CodOcorrencia);

               if ((CodOcorrencia >= 5) and (CodOcorrencia <= 8)) or (CodOcorrencia = 15) or (CodOcorrencia = 46) then
               begin
                    CodigoLiquidacao := Copy(Linha, 109, 2);
                    CodigoLiquidacaoDescricao := TipoOcorrenciaToDescricao(OcorrenciaOriginal.Tipo);
               end;

               if (CodOcorrencia >= 2) and ((CodOcorrencia <= 10)) then
               begin

                    CodMotivo := StrToIntDef(Copy(Linha, 87, 2), 0);
                    MotivoRejeicaoComando.Add(Copy(Linha, 87, 2));
                    DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo, CodMotivo));
               end;

               DataOcorrencia := StringToDateTimeDef(Copy(Linha, 111, 2) + '/' +
                                 Copy(Linha, 113, 2) + '/' + Copy(Linha, 115, 2), 0, 'DD/MM/YY');

               Vencimento := StringToDateTimeDef(Copy(Linha, 147, 2) + '/' +
                             Copy(Linha, 149, 2) + '/' + Copy(Linha, 151, 2), 0, 'DD/MM/YY');

               ValorDocumento := StrToFloatDef(Copy(Linha, 153, 13), 0) / 100;
               ValorIOF := StrToFloatDef(Copy(Linha, 215, 13), 0) / 100;
               ValorAbatimento := StrToFloatDef(Copy(Linha, 228, 13), 0) / 100;
               ValorDesconto := StrToFloatDef(Copy(Linha, 241, 13), 0) / 100;
               ValorRecebido := StrToFloatDef(Copy(Linha, 254, 13), 0) / 100;
               ValorMoraJuros := StrToFloatDef(Copy(Linha, 267, 13), 0) / 100;
               ValorOutrosCreditos := StrToFloatDef(Copy(Linha, 280, 13), 0) / 100;
               Carteira := Copy(Linha, 107, 2);
               NossoNumero := Copy(Linha, 71, 10);
               ValorDespesaCobranca := StrToFloatDef(Copy(Linha, 182, 07), 0) / 100;
               ValorOutrasDespesas := StrToFloatDef(Copy(Linha, 189, 13), 0) / 100;

               if StrToIntDef(Copy(Linha, 176, 6), 0) <> 0 then
                    DataCredito := StringToDateTimeDef(Copy(Linha, 176, 2) + '/' +
                    Copy(Linha, 178, 2) + '/' + Copy(Linha, 180, 2), 0, 'DD/MM/YY');
          end;
     end;
     fpTamanhoMaximoNossoNum := 10;
end;

end.
