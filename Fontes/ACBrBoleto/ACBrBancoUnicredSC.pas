{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2009 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  Juliana Rodrigues Prado                        }
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

unit ACBrBancoUnicredSC;

interface

uses
  Classes, SysUtils, ACBrBoleto, ACBrBancoUnicredRS;

type

  { TACBrBancoUnicredSC }

  TACBrBancoUnicredSC = class(TACBrBancoUnicredRS)
  protected
    function DefineEspecieDoc(const ACBrTitulo: TACBrTitulo): String;
    function DefineCampoLivreCodigoBarras(const ACBrTitulo: TACBrTitulo):String; override;

  public
    Constructor create(AOwner: TACBrBanco);
    procedure GerarRegistroTransacao400(ACBrTitulo : TACBrTitulo; aRemessa: TStringList); override;

  end;

implementation

uses {$IFDEF COMPILER6_UP} dateutils {$ELSE} ACBrD5 {$ENDIF},
  StrUtils, ACBrUtil ;

{ TACBrBancoUnicredSC }

constructor TACBrBancoUnicredSC.create(AOwner: TACBrBanco);
begin
   inherited create(AOwner);
   fpDigito                 := 2;
   fpNome                   := 'BRADESCO';
   fpNumero                 := 237;
   fpTamanhoMaximoNossoNum  := 11;
   fpTamanhoAgencia         := 4;
   fpTamanhoConta           := 7;
   fpTamanhoCarteira        := 3;
   fpCodParametroMovimento  := 'MX';
   fpModuloMultiplicadorFinal := 7;

end;

function TACBrBancoUnicredSC.DefineEspecieDoc(const ACBrTitulo: TACBrTitulo): String;
begin
  with ACBrTitulo do
  begin
    if trim(EspecieDoc) = 'DM' then
       Result:= '01'
    else if trim(EspecieDoc) = 'NP' then
       Result:= '02'
    else if trim(EspecieDoc) = 'NS' then
       Result:= '03'
    else if trim(EspecieDoc) = 'CS' then
       Result:= '04'
    else if trim(EspecieDoc) = 'REC' then
       Result:= '05'
    else if trim(EspecieDoc) = 'LC' then
       Result:= '10'
    else if trim(EspecieDoc) = 'ND' then
       Result:= '11'
    else if trim(EspecieDoc) = 'DS' then
       Result:= '12'
    else if trim(EspecieDoc) = 'OU' then
       Result:= '99'
    else
       Result := EspecieDoc;
  end;

end;

function TACBrBancoUnicredSC.DefineCampoLivreCodigoBarras(
  const ACBrTitulo: TACBrTitulo): String;
begin
  with ACBrTitulo.ACBrBoleto do
  begin
    Result := PadLeft(OnlyNumber(Cedente.Agencia), fpTamanhoAgencia, '0') +{20-23: Campo Livre 1 - Agência }
              PadLeft(copy(ACBrTitulo.Carteira,2,2),2,'0') +               {24-25: Carteira }
              PadLeft(ACBrTitulo.NossoNumero,11,'0') +                     {26-36: Campo Livre 3 - Nosso Núm }
              PadLeft(RightStr(Cedente.Conta,7),7,'0')+                    {37-43: Conta do Benificiário}
              '0';                                                         {44-44: Zero}
  end;
end;

procedure TACBrBancoUnicredSC.GerarRegistroTransacao400(ACBrTitulo :TACBrTitulo; aRemessa: TStringList);
var
  sDigitoNossoNumero, sOcorrencia, sEspecie, sAgencia : String;
  sProtesto, sTipoSacado, sMensagemCedente, sConta    : String;
  sLinha, sNossoNumero : String;
begin
   with ACBrTitulo do
   begin
     ValidaNossoNumeroResponsavel(sNossoNumero, sDigitoNossoNumero, ACBrTitulo);
     sAgencia      := IntToStrZero(StrToIntDef(OnlyNumber(ACBrBoleto.Cedente.Agencia), 0), 4);
     sConta        := IntToStrZero(StrToIntDef(OnlyNumber(ACBrBoleto.Cedente.Conta)  , 0), 7);

     {Pegando Código da Ocorrencia}
     sOcorrencia:= TipoOCorrenciaRemessaToCod(OcorrenciaOriginal.Tipo);

     {Pegando Especie}
     sEspecie:= DefineEspecieDoc(ACBrTitulo);

     {Pegando campo Intruções}
      sProtesto:= InstrucoesProtesto(ACBrTitulo);

     {Pegando Tipo de Sacado}
      sTipoSacado := DefineTipoSacado(ACBrTitulo);

     with ACBrBoleto do
     begin
       if Mensagem.Text <> '' then
          sMensagemCedente:= Mensagem[0];

       sLinha:= '1'                                                     + {001-001: ID Registro }
                space(19)                                               + {002-020: Espaço Vazio - Caso débito automático deve preenxer}
                '0'                                                     + {021-021: Fixar Zero}
                '009'                                                   + {022-024: Fixar ‘009’ (Cobrança Registrado)}
                '0'                                                     + {025-025: Fixar Zero}
                sAgencia                                                + {026-029: Agência }
                PadLeft(sConta, 07, '0')                                + {030-036: Conta Corrente }
                Cedente.ContaDigito                                     + {037-037: Conta Corrente Dígito }
                PadRight(SeuNumero, 25, ' ')                            + {038-062: Numero Controle do Participante }
                '000'                                                   + {063-065: Espaço Vazio - caso débito automático preenxer com 237}

                IfThen(PercentualMulta > 0, '2', '0')                   + {066-066: Indicação de multa}
                IfThen(PercentualMulta > 0,
                                  IntToStrZero(Round(PercentualMulta * 100), 4),
                                  PadRight('', 4, '0')) +                 {067-070: Percentual da multa }
                PadLeft(sNossoNumero, 11, '0')                          + {071-081: Nosso Número }
                PadLeft(sDigitoNossoNumero, 1, '0')                     + {082-082: Digito Verificador do Nosso Número }
                IntToStrZero( round( ValorDescontoAntDia * 100), 10)    + {083-092: Desconto bonificação por dia}
                '2'                                                     + {093-093: Quem emite o boleto}
                ' '                                                     + {094-094: Identificação se emite boleto para débito automático}
                Space(10)                                               + {095-104: Identificação da operação no banco}
                ' '                                                     + {105-105: Indicador Rateio de Credito }
                '2'                                                     + {106-106: Não emite aviso de  Débito Automático em Conta Corrente}
                Space(2)                                                + {107-108: Quantidade possíveis de pagamento}
                sOcorrencia                                             + {109-110: Ocorrência}
                PadRight(NumeroDocumento, 10)                           + {111-120: Número DOcumento }
                FormatDateTime('ddmmyy', Vencimento)                    + {121-126: Vencimento }
                IntToStrZero(Round(ValorDocumento * 100 ), 13)          + {127-139: Valor do Título }
                '000'                                                   + {140-142: Banco Encarregado da Cobrança}
                '00000'                                                 + {143-147: Agência Depositária}
                sEspecie                                                + {148-149: Especie}
                'N'                                                     + {150-150: Aceite, Sempre com 'N'}
                FormatDateTime('ddmmyy', DataDocumento)                 + {151-156: Data de Emissão }
                sProtesto                                               + {157-158:159-160 Protesto-Quantidade de dias }
                IntToStrZero(Round(ValorMoraJuros * 100), 13)           + {161-173: Valor por dia de Atraso }
                IfThen(DataDesconto < 0, '000000',
                       FormatDateTime('ddmmyy', DataDesconto))          + {174-179: Data Limite Desconto }
                IntToStrZero(Round(ValorDesconto * 100), 13)            + {180-192: Valor Desconto }
                IntToStrZero( round( ValorIOF * 100 ), 13)              + {193-205: Valor IOF }
                IntToStrZero(Round(ValorAbatimento * 100), 13)          + {206-218: Valor Abatimento }
                sTipoSacado                                             + {219-220: Tipo Inscrição Sacado }
                PadLeft(OnlyNumber(Sacado.CNPJCPF), 14, '0')            + {221-234: Núm. Incrição Sacado }
                PadRight(Sacado.NomeSacado, 40, ' ')                    + {235-274: Nome do Sacado }
                PadRight(Sacado.Logradouro + ' ' + Sacado.Numero, 40)   + {275-314: Endereço do Sacado }
                PadRight(sMensagemCedente, 12)                          + {315-326: 1ª Mensagem}
                PadRight(Sacado.CEP, 8, ' ')                            + {327-334: CEP do Sacado }
                PadRight(Sacado.Bairro, 20, ' ')                        + {335-354: Bairro do Sacado }
                PadRight(Sacado.Cidade, 38, ' ')                        + {355-392: Cidade do Sacado }
                PadRight(Sacado.UF, 2, ' ')                             + {393-394: UF Cidade do Sacado }
                IntToStrZero(aRemessa.Count + 1, 6);                      {395-400: Núm Sequencial arquivo }

       aRemessa.Add(UpperCase(sLinha));

       sLinha := MontaInstrucoes(ACBrTitulo, aRemessa.Count );
       if not(sLinha = EmptyStr) then
         aRemessa.Add(UpperCase(sLinha));
     end;
   end;

end;

end.


