{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliana R P Tamizou                             }
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

unit ACBrBancoPineBradesco;

interface

uses
  Classes, Contnrs, SysUtils, ACBrBoleto, ACBrBancoPine;

type


  { TACBrBancoPineBradesco }

  TACBrBancoPineBradesco = class(TACBrBancoPine)
  private
    function TipoOcorrenciaRemessaToCod(const TipoOcorrencia: TACBrTipoOcorrencia): String;
    function EspecieDocToCod(const Especie: String): String;
    function CodToEspecie(const CodEspecie: Integer): String;
    function MontarPosicoesSacador(const ACBrTitulo:TACBrTitulo; const Instrucao94: Boolean): String;
    function MontarRegistroMensagens(const ACBrTitulo:TACBrTitulo; const Instrucao94: Boolean): String;
    function MontarRegistroSacador(const ACBrTitulo:TACBrTitulo): String;
    procedure GerarRegistrosNFe(ACBrTitulo : TACBrTitulo; aRemessa: TStringList);
  protected
    function CalcularNossoNumero(const ACBrTitulo:TACBrTitulo): String; override;
    function CalcularCarteira(const ACBrTitulo:TACBrTitulo): String; override;
    function LerNossoNumero(const aLinha:String; ACBrTitulo : TACBrTitulo ):String;  override;
  public
    Constructor create(AOwner: TACBrBanco);
    function CalcularDigitoVerificador(const ACBrTitulo:TACBrTitulo): String; override;
    function MontarCodigoBarras(const ACBrTitulo : TACBrTitulo): String; override;
    function MontarCampoNossoNumero(const ACBrTitulo :TACBrTitulo): String; override;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String; override;
  end;

implementation

uses {$IFDEF COMPILER6_UP} dateutils {$ELSE} ACBrD5 {$ENDIF},
  StrUtils,
  ACBrUtil, ACBrValidador ;

{ TACBrBancoPineBradesco }

function TACBrBancoPineBradesco.TipoOcorrenciaRemessaToCod(const TipoOcorrencia: TACBrTipoOcorrencia): String;
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

function TACBrBancoPineBradesco.EspecieDocToCod(const Especie: String): String;
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

function TACBrBancoPineBradesco.CodToEspecie(const CodEspecie: Integer): String;
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

function TACBrBancoPineBradesco.MontarPosicoesSacador(const ACBrTitulo: TACBrTitulo;
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

function TACBrBancoPineBradesco.MontarRegistroMensagens(const ACBrTitulo: TACBrTitulo;
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

function TACBrBancoPineBradesco.MontarRegistroSacador(const ACBrTitulo: TACBrTitulo
  ): String;
var
  wTipoSacador: String;
begin
  if  ACBrTitulo.Sacado.SacadoAvalista.Pessoa = pFisica then
     wTipoSacador := '01'
  else
     wTipoSacador := '02';

  Result:= sLineBreak + '5'       +
           StringOfChar(' ',120)  +
           wTipoSacador           +
           PadLeft(ACBrTitulo.Sacado.SacadoAvalista.CNPJCPF, 14, '0')             +
           PadRight(ACBrTitulo.Sacado.SacadoAvalista.Logradouro + ' ' +
                    ACBrTitulo.Sacado.SacadoAvalista.Numero + ' ' +
                    ACBrTitulo.Sacado.SacadoAvalista.Complemento,40)              +
           PadRight(ACBrTitulo.Sacado.SacadoAvalista.Bairro,12)                   +
           PadLeft(ACBrTitulo.Sacado.SacadoAvalista.CEP, 8, '0')                  +
           PadRight(ACBrTitulo.Sacado.SacadoAvalista.Cidade,15)                   +
           PadRight(ACBrTitulo.Sacado.SacadoAvalista.UF,2)                        +
           StringOfChar(' ', 180);
end;

function TACBrBancoPineBradesco.CalcularNossoNumero(
  const ACBrTitulo: TACBrTitulo): String;
begin
  Result:=  StringOfChar('0', 11); {63 a 73 - Nosso Numero e DV Pine}

  if ACBrTitulo.ACBrBoleto.Cedente.ResponEmissao = tbCliEmite then
     Result:= Result + PadLeft(ACBrTitulo.NossoNumero +
                       CalcularDigitoVerificador(ACBrTitulo),13, '0')
  else
     Result:= Result + StringOfChar('0',13);

  Result:= Result + Space(3);
end;

function TACBrBancoPineBradesco.CalcularCarteira(const ACBrTitulo: TACBrTitulo
  ): String;
begin
  if ACBrBanco.ACBrBoleto.Cedente.ResponEmissao = tbCliEmite then
      Result:= '4'
   else
      Result:= '7';
end;

function TACBrBancoPineBradesco.LerNossoNumero(const aLinha: String;
  ACBrTitulo: TACBrTitulo): String;
begin
  Result:=inherited LerNossoNumero(aLinha, ACBrTitulo);
  ACBrTitulo.NossoNumeroCorrespondente:= Copy(aLinha, 95, TamanhoMaximoNossoNum);
end;

procedure TACBrBancoPineBradesco.GerarRegistrosNFe(ACBrTitulo: TACBrTitulo;
  aRemessa: TStringList);
var
  wQtdRegNFes, J, I: Integer;
  wLinha: String;
  Continua: Boolean;
begin
   wQtdRegNFes:= round(ACBrTitulo.ListaDadosNFe.Count / 3);

   if (ACBrTitulo.ListaDadosNFe.Count mod 3) <> 0 then
      Inc(wQtdRegNFes);

   J:= 0;
   I:= 0;
   repeat
   begin
      Continua:=  true;

      wLinha:= wLinha + '4';

      while (Continua) and (J < ACBrTitulo.ListaDadosNFe.Count) do
      begin
         wLinha:= wLinha +
                  PadRight(ACBrTitulo.ListaDadosNFe[J].NumNFe,15) +
                  IntToStrZero( round(ACBrTitulo.ListaDadosNFe[J].ValorNFe  * 100 ), 13) +
                  FormatDateTime('ddmmyyyy',ACBrTitulo.ListaDadosNFe[J].EmissaoNFe)      +
                  PadLeft(ACBrTitulo.ListaDadosNFe[J].ChaveNFe, 44, '0');

         Inc(J);
         Continua:= (J mod 3) <> 0 ;
      end;

      wLinha:= PadRight(wLinha,241) + StringOfChar(' ', 153) +
               IntToStrZero(aRemessa.Count + 1, 6);

      aRemessa.Add(wLinha);
      Inc(I);
   end;
   until (I = wQtdRegNFes) ;
end;

constructor TACBrBancoPineBradesco.create(AOwner: TACBrBanco);
begin
   inherited create(AOwner);
   fpNome                   := 'Bradesco';
   fpNumero                 := 237;
   fpTamanhoMaximoNossoNum  := 11;
   fpTamanhoAgencia         := 4;
   fpTamanhoConta           := 6;
   fpTamanhoCarteira        := 2;
   fCorrespondente          := True;
   fpNumeroCorrespondente   := 643;
end;

function TACBrBancoPineBradesco.CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo ): String;
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

function TACBrBancoPineBradesco.MontarCodigoBarras ( const ACBrTitulo: TACBrTitulo) : String;
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

function TACBrBancoPineBradesco.MontarCampoNossoNumero (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
   Result:= ACBrTitulo.Carteira+'/'+ACBrTitulo.NossoNumero+'-'+CalcularDigitoVerificador(ACBrTitulo);
end;

function TACBrBancoPineBradesco.MontarCampoCodigoCedente (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
   Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia+'-'+
             ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito+'/'+
             ACBrTitulo.ACBrBoleto.Cedente.Conta+'-'+
             ACBrTitulo.ACBrBoleto.Cedente.ContaDigito;
end;

end.



