{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

unit ACBrPagForLerTxt;

interface

uses
  SysUtils, Classes, ACBrPagForClass, ACBrPagForConversao;

type
  TPagForR = class(TPersistent)
  private
    FPagFor: TPagFor;
    FArquivoTXT: TStringList;
    FAtivo: Boolean;

    procedure LerRegistro0;
    procedure LerRegistro1(I: Integer);
    procedure LerSegmentoA(I: Integer);
    procedure LerSegmentoB(mSegmentoBList: TSegmentoBList; I:Integer);
    procedure LerSegmentoC(mSegmentoCList: TSegmentoCList; I:Integer);
    procedure LerSegmentoD(mSegmentoDList: TSegmentoDList; I:Integer);
    procedure LerSegmentoE(mSegmentoEList: TSegmentoEList; I:Integer);
    procedure LerSegmentoF(mSegmentoFList: TSegmentoFList; I:Integer);
    procedure LerSegmentoG(I: Integer);
    procedure LerSegmentoH(mSegmentoHList: TSegmentoHList; I:Integer);
    procedure LerSegmentoJ(I: Integer; var LeuRegistroJ: boolean);
    procedure LerSegmentoJ52(mSegmentoJ52List: TSegmentoJ52List; I:Integer);
    procedure LerSegmentoJ99(mSegmentoJ99List: TSegmentoJ99List; I:Integer);
    procedure LerSegmentoN1(I: Integer);
    procedure LerSegmentoN2(I: Integer);
    procedure LerSegmentoN3(I: Integer);
    procedure LerSegmentoN4(I: Integer);
    procedure LerSegmentoN567(I: Integer);
    procedure LerSegmentoN8(I: Integer);
    procedure LerSegmentoN9(I: Integer);
    procedure LerSegmentoO(I: Integer);
    procedure LerSegmentoP(I: Integer);
    procedure LerSegmentoQ(I: Integer);
    procedure LerSegmentoR(I: Integer);
    procedure LerSegmentoS(I: Integer);
    procedure LerSegmentoW(mSegmentoWList: TSegmentoWList; I:Integer);
    procedure LerSegmentoY(I: Integer);
    procedure LerSegmentoZ(mSegmentoZList: TSegmentoZList; I:Integer);
    procedure LerRegistro5(I: Integer);
    procedure LerRegistro9(I: Integer);

    procedure LerLote;
  public
    constructor Create(AOwner: TPagFor);
    destructor Destroy; override;
    function LerTXT(const ArquivoTXT: string): Boolean;

  published
    property PagFor: TPagFor read FPagFor write FPagFor;
  end;

implementation

uses
  ACBrUtil.DateTime;

{ TPagForW }

constructor TPagForR.Create(AOwner: TPagFor);
begin
  inherited Create;
  FPagFor       := AOwner;
  FArquivoTXT := TStringList.Create;
  FAtivo      := True;
end;

destructor TPagForR.Destroy;
begin
  FAtivo := False;
  FArquivoTXT.Free;
  inherited Destroy;
end;

procedure TPagForR.LerLote;
var
  I:integer;
  // Necessario para ler os segmentos opcionais (B, C, etc)
  // apenas se eles pertencem a um segmento J
  LeuRegistroJ: boolean;
begin
  LeuRegistroJ := False;
  try
    for I := 1 to FArquivoTXT.Count - 1 do
    begin
      if Copy(FArquivoTXT.Strings[i], 8, 1) = '1' then {Tipo de registro = 1}
      begin
        LerRegistro1(I);
        LeuRegistroJ := False; // Sempre reseta em um novo lote
      end;

      if (Copy(FArquivoTXT.Strings[i], 8, 1) <> '1') and
         (Copy(FArquivoTXT.Strings[i], 8, 1) <> '5') and
         (Copy(FArquivoTXT.Strings[i], 8, 1) <> '9') then
      begin
        LerSegmentoA(I);
        LerSegmentoG(I);

        if FPagFor.Lote.Last.SegmentoG.Count > 0 then
          LerSegmentoH(FPagFor.Lote.Last.SegmentoG.Last.SegmentoH, i);

        LerSegmentoJ(I, LeuRegistroJ);
        LerSegmentoN1(I);
        LerSegmentoN2(I);
        LerSegmentoN3(I);
        LerSegmentoN4(I);
        LerSegmentoN567(I);
        LerSegmentoN8(I);
        LerSegmentoN9(I);
        LerSegmentoO(I);
        LerSegmentoP(I);
        LerSegmentoQ(I);
        LerSegmentoR(I);
        LerSegmentoS(I);
        LerSegmentoY(I);
      end;

      if Copy(FArquivoTXT.Strings[i], 8, 1) = '5' then {Tipo de registro = 5}
        LerRegistro5(I);
    end;
  except
    on E: Exception do
    begin
      raise Exception.Create('Não Foi Possível ler os registros no arquivo.' + #13 + E.Message);
    end;
  end;
end;

procedure TPagForR.LerRegistro0;
var
  mOk:boolean;
begin
  FPagFor.Registro0.Aviso.Clear;

  FPagFor.Geral.Banco                        := StrToBanco(mOk, Copy(FArquivoTXT.Strings[0], 1, 3));
  FPagFor.Registro0.Empresa.Inscricao.Tipo   := StrToTpInscricao(mOk, Copy(FArquivoTXT.Strings[0], 18, 1));
  FPagFor.Registro0.Empresa.Inscricao.Numero := Copy(FArquivoTXT.Strings[0], 19, 14);
  FPagFor.Registro0.Empresa.Convenio         := Trim(Copy(FArquivoTXT.Strings[0], 33, 20));

  case FPagFor.Geral.Banco of
    pagBancoDoBrasil,
    pagItau,
    pagSicredi,
    pagBancoCECRED,
    pagSantander,
    pagBradesco:
      begin
        FPagFor.Registro0.Empresa.ContaCorrente.Agencia.Codigo := StrToInt(Copy(FArquivoTXT.Strings[0], 53, 5));
        FPagFor.Registro0.Empresa.ContaCorrente.Agencia.DV     := Copy(FArquivoTXT.Strings[0], 58, 1);
        FPagFor.Registro0.Empresa.ContaCorrente.Conta.Numero   := StrToInt(Copy(FArquivoTXT.Strings[0], 59, 12));
        FPagFor.Registro0.Empresa.ContaCorrente.Conta.DV       := Copy(FArquivoTXT.Strings[0], 71, 1);
        FPagFor.Registro0.Empresa.ContaCorrente.DV             := Copy(FArquivoTXT.Strings[0], 72, 1);
      end;
  end;

  FPagFor.Registro0.Empresa.Nome        := Trim(Copy(FArquivoTXT.Strings[0], 73, 30));
  FPagFor.Registro0.NomeBanco           := Trim(Copy(FArquivoTXT.Strings[0], 103, 30));
  FPagFor.Registro0.Arquivo.Codigo      := StrToTpArquivo(mOk, Copy(FArquivoTXT.Strings[0], 143, 1));
  FPagFor.Registro0.Arquivo.DataGeracao := StringToDateTime(Copy(FArquivoTXT.Strings[0], 144, 2)+'/'+Copy(FArquivoTXT.Strings[0], 146, 2)+'/'+Copy(FArquivoTXT.Strings[0], 148, 4));
  FPagFor.Registro0.Arquivo.HoraGeracao := StringToDateTimeDef(Copy(FArquivoTXT.Strings[0], 152, 2)+':'+Copy(FArquivoTXT.Strings[0], 154, 2)+':'+Copy(FArquivoTXT.Strings[0], 156, 2),StrToTime('00:00:00'));
  FPagFor.Registro0.Arquivo.Sequencia   := StrToInt(Copy(FArquivoTXT.Strings[0], 158, 6));

  if FPagFor.Geral.Banco = pagBancoDoBrasil then
  begin
    FPagFor.Registro0.Arquivo.Densidade := StrToInt(Copy(FArquivoTXT.Strings[0], 167, 5));
    fpagfor.registro0.ReservadoBanco := Copy(FArquivoTXT.Strings[0], 181, 11); //PREVIA/ PROCESSAMEN (base para liquidação no ERP)/ CONSOLIDADO/
  end;

end;

procedure TPagForR.LerRegistro1(I: Integer);
var
  mOk: Boolean;
  ajusteBloqueto: Integer;
begin
  ajusteBloqueto := 0;

  FPagFor.Lote.New;
  FPagFor.Lote.Last.Registro1.Servico.Operacao    := StrToTpOperacao(mOk, Copy(FArquivoTXT.Strings[i], 9, 1));
  FPagFor.Lote.Last.Registro1.Servico.TipoServico := StrToTpServico(mOk, Copy(FArquivoTXT.Strings[i], 10, 2));

  //Quando é bloqueto Eletrônico o campo do CNPJ da empresa possui 15 caracteres
  //e para os demais serviços possui 14
  if FPagFor.Lote.Last.Registro1.Servico.TipoServico = tsBloquetoEletronico then
    ajusteBloqueto := 1;

  FPagFor.Lote.Last.Registro1.Servico.FormaLancamento  := StrToFmLancamento(mOk, Copy(FArquivoTXT.Strings[i], 12, 2));
  FPagFor.Lote.Last.Registro1.Empresa.Inscricao.Tipo   := StrToTpInscricao(mOk, Copy(FArquivoTXT.Strings[i], 18, 1));
  FPagFor.Lote.Last.Registro1.Empresa.Inscricao.Numero := Copy(FArquivoTXT.Strings[i], 19, 14 + ajusteBloqueto);
  FPagFor.Lote.Last.Registro1.Empresa.Convenio         := Trim(Copy(FArquivoTXT.Strings[i], 33 + ajusteBloqueto, 20));

  case FPagFor.Geral.Banco of
    pagBancoDoBrasil,
    pagItau,
    pagSicredi,
    pagBancoCECRED,
    pagSantander,
    pagBradesco:
      begin
        FPagFor.Lote.Last.Registro1.Empresa.ContaCorrente.Agencia.Codigo := StrToInt(Copy(FArquivoTXT.Strings[i], 53 + ajusteBloqueto, 5));
        FPagFor.Lote.Last.Registro1.Empresa.ContaCorrente.Agencia.DV     := Copy(FArquivoTXT.Strings[i], 58 + ajusteBloqueto, 1);
        FPagFor.Lote.Last.Registro1.Empresa.ContaCorrente.Conta.Numero   := StrToInt(Copy(FArquivoTXT.Strings[i], 59 + ajusteBloqueto, 12));
        FPagFor.Lote.Last.Registro1.Empresa.ContaCorrente.Conta.DV       := Copy(FArquivoTXT.Strings[i], 71 + ajusteBloqueto, 1);
        FPagFor.Lote.Last.Registro1.Empresa.ContaCorrente.DV             := Copy(FArquivoTXT.Strings[i], 72 + ajusteBloqueto, 1);
      end;
  end;

  FPagFor.Lote.Last.Registro1.Empresa.Nome := Trim(Copy(FArquivoTXT.Strings[i], 73 + ajusteBloqueto, 30));

  if FPagFor.Lote.Last.Registro1.Servico.TipoServico = tsBloquetoEletronico then
    Exit;

  FPagFor.Lote.Last.Registro1.Informacao1          := Copy(FArquivoTXT.Strings[i], 103, 40);
  FPagFor.Lote.Last.Registro1.Endereco.Logradouro  := Copy(FArquivoTXT.Strings[i], 143, 30);
  FPagFor.Lote.Last.Registro1.Endereco.Numero      := StrToIntDef(Trim(Copy(FArquivoTXT.Strings[i], 173, 5)), 0);
  FPagFor.Lote.Last.Registro1.Endereco.Complemento := Copy(FArquivoTXT.Strings[i], 178, 15);
  FPagFor.Lote.Last.Registro1.Endereco.Cidade      := Copy(FArquivoTXT.Strings[i], 193, 20);
  FPagFor.Lote.Last.Registro1.Endereco.CEP         := StrToIntDef(Copy(FArquivoTXT.Strings[i], 213, 8),0);
  FPagFor.Lote.Last.Registro1.Endereco.Estado      := Copy(FArquivoTXT.Strings[i], 221, 2);
end;

procedure TPagForR.LerRegistro5(I: Integer);
begin
  case FPagFor.Geral.Banco of
    pagItau:
      begin
        if (FPagFor.Lote.Last.Registro1.Servico.FormaLancamento = flPagamentoConcessionarias) then
        begin // Contas de Concessionárias e Tributos com código de barras
          FPagFor.Lote.Last.Registro5.Valor     := StrToInt(Copy(FArquivoTXT.Strings[i], 24, 18)) / 100;
          FPagFor.Lote.Last.Registro5.QtdeMoeda := StrToInt(Copy(FArquivoTXT.Strings[i], 42, 15)) / 100000000;
        end
        else if (FPagFor.Lote.Last.Registro1.Servico.TipoServico = tsPagamentoSalarios) or (Copy(FArquivoTXT.Strings[i], 60, 1) <> ' ') then
        begin // fgts
          FPagFor.Lote.Last.Registro5.Valor                := StrToInt(Copy(FArquivoTXT.Strings[i], 24, 14)) / 100;
          FPagFor.Lote.Last.Registro5.TotalOutrasEntidades := StrToIntDef(Copy(FArquivoTXT.Strings[i], 38, 14), 0) / 100;
          FPagFor.Lote.Last.Registro5.TotalValorAcrescimo  := StrToInt(Copy(FArquivoTXT.Strings[i], 52, 14)) / 100;
          FPagFor.Lote.Last.Registro5.TotalValorArrecadado := StrToInt(Copy(FArquivoTXT.Strings[i], 66, 14)) / 100;
        end
        else
        begin
          // Pagamentos através de cheque, OP, DOC, TED e crédito em conta corrente
          // Liquidação de títulos (bloquetos) em cobrança no Itaú e em outros Bancos
          FPagFor.Lote.Last.Registro5.Valor := StrToInt(Copy(FArquivoTXT.Strings[i], 24, 18)) / 100;
        end;
      end;
  end;
end;

procedure TPagForR.LerRegistro9(I: Integer);
begin
  FPagFor.Registro9.Totais.QtdeLotes     := StrToInt(Copy(FArquivoTXT.Strings[i], 18, 6));
  FPagFor.Registro9.Totais.QtdeRegistros := StrToInt(Copy(FArquivoTXT.Strings[i], 24, 6));
//  FPagFor.Registro9.Totais.QtdeContasConciliadas := Copy(FArquivoTXT.Strings[i], 0, 0);
end;

procedure TPagForR.LerSegmentoA(I: Integer);
var
  mOk:boolean;
  x: Integer;
begin
  if ((Copy(FArquivoTXT.Strings[i], 8, 1) + Copy(FArquivoTXT.Strings[i], 14, 1)) <> '3A') then
    Exit;

  FPagFor.Lote.Last.SegmentoA.New;
  FPagFor.Lote.Last.SegmentoA.Last.TipoMovimento     := StrToTpMovimento(mOk, Copy(FArquivoTXT.Strings[i], 15, 1));
  FPagFor.Lote.Last.SegmentoA.Last.CodMovimento      := StrToInMovimento(mOk, Copy(FArquivoTXT.Strings[i], 16, 2));
  FPagFor.Lote.Last.SegmentoA.Last.Favorecido.Camara := StrToInt(Copy(FArquivoTXT.Strings[i], 18, 3));
  FPagFor.Lote.Last.SegmentoA.Last.Favorecido.Banco  := StrToBanco(mOk, Copy(FArquivoTXT.Strings[i], 21, 3));

  case FPagFor.Geral.Banco of
    pagItau:
      begin
        FPagFor.Lote.Last.SegmentoA.Last.Favorecido.ContaCorrente.Agencia.Codigo := StrToInt(Copy(FArquivoTXT.Strings[i], 24, 5));
        FPagFor.Lote.Last.SegmentoA.Last.Favorecido.ContaCorrente.Conta.Numero   := StrToInt64(Copy(FArquivoTXT.Strings[i], 30, 12));

        if Copy(FArquivoTXT.Strings[i], 31, 1) <> ' ' then
          FPagFor.Lote.Last.SegmentoA.Last.Favorecido.ContaCorrente.Agencia.DV := Copy(FArquivoTXT.Strings[i], 31, 1);

        FPagFor.Lote.Last.SegmentoA.Last.Favorecido.ContaCorrente.Conta.DV := Copy(FArquivoTXT.Strings[i], 32, 1);
      end;

    pagSicredi,
    pagSantander,
    pagBancoDoBrasil:
      begin
        FPagFor.Lote.Last.SegmentoA.Last.Favorecido.ContaCorrente.Agencia.Codigo := StrToInt(Copy(FArquivoTXT.Strings[i], 24, 5));
        FPagFor.Lote.Last.SegmentoA.Last.Favorecido.ContaCorrente.Agencia.DV     := Copy(FArquivoTXT.Strings[i], 29, 1);
        FPagFor.Lote.Last.SegmentoA.Last.Favorecido.ContaCorrente.Conta.Numero   := StrToInt64(Copy(FArquivoTXT.Strings[i], 30, 12));
        FPagFor.Lote.Last.SegmentoA.Last.Favorecido.ContaCorrente.Conta.DV       := Copy(FArquivoTXT.Strings[i], 42, 1);
      end;
  end;

  FPagFor.Lote.Last.SegmentoA.Last.Favorecido.Nome       := Copy(FArquivoTXT.Strings[i], 44, 30);
  FPagFor.Lote.Last.SegmentoA.Last.Credito.SeuNumero     := Copy(FArquivoTXT.Strings[i], 74, 20);
  FPagFor.Lote.Last.SegmentoA.Last.Credito.DataPagamento := StringToDateTime(Copy(FArquivoTXT.Strings[i], 94, 2)+'/'+Copy(FArquivoTXT.Strings[i], 96, 2)+'/'+Copy(FArquivoTXT.Strings[i], 98, 4));

  case FPagFor.Geral.Banco of
    pagItau,
    pagSicredi,
    pagSantander:
      begin
        FPagFor.Lote.Last.SegmentoA.Last.Credito.ValorPagamento := StrToInt(Copy(FArquivoTXT.Strings[i], 120, 15)) / 100;
        FPagFor.Lote.Last.SegmentoA.Last.Credito.NossoNumero    := Copy(FArquivoTXT.Strings[i], 135, 15);
        FPagFor.Lote.Last.SegmentoA.Last.Credito.DataReal       := StringToDateTimeDef(Copy(FArquivoTXT.Strings[i], 155, 2)+'/'+Copy(FArquivoTXT.Strings[i], 157, 2)+'/'+Copy(FArquivoTXT.Strings[i], 159, 4), 0);
        FPagFor.Lote.Last.SegmentoA.Last.Credito.ValorReal      := StrToInt(Copy(FArquivoTXT.Strings[i], 163, 15)) / 100;

        if FPagFor.Geral.Banco = pagItau then
        begin
          FPagFor.Lote.Last.SegmentoA.Last.Informacao2 := Copy(FArquivoTXT.Strings[i], 178, 20);
          FPagFor.Lote.Last.SegmentoA.Last.NumeroDocumento := StrToIntDef(Copy(FArquivoTXT.Strings[i], 198, 6), 0);
          FPagFor.Lote.Last.SegmentoA.Last.Favorecido.Inscricao.Numero := Copy(FArquivoTXT.Strings[i], 204, 14);
        end
        else
          FPagFor.Lote.Last.SegmentoA.Last.Informacao2 := Copy(FArquivoTXT.Strings[i], 178, 40);

        FPagFor.Lote.Last.SegmentoA.Last.CodigoDOC     := Copy(FArquivoTXT.Strings[i], 218, 2);
        FPagFor.Lote.Last.SegmentoA.Last.CodigoTED     := Copy(FArquivoTXT.Strings[i], 220, 5);
        FPagFor.Lote.Last.SegmentoA.Last.Aviso         := StrToIntDef(Copy(FArquivoTXT.Strings[i], 230, 1),0);
        FPagFor.Lote.Last.SegmentoA.Last.CodOcorrencia := Trim(Copy(FArquivoTXT.Strings[i], 231, 10));

        case FPagFor.Geral.Banco of
          pagItau,
          pagBradesco:
            FPagFor.Lote.Last.SegmentoA.Last.DescOcorrencia := DescricaoRetornoItau(FPagFor.Lote.Last.SegmentoA.Last.CodOcorrencia);

          pagSantander:
            FPagFor.Lote.Last.SegmentoA.Last.DescOcorrencia := DescricaoRetornoSantander(FPagFor.Lote.Last.SegmentoA.Last.CodOcorrencia);
        end;

        if POS(FPagFor.Lote.Last.SegmentoA.Last.CodOcorrencia, PAGAMENTO_LIBERADO_AVISO) = 0 then
        begin
          FPagFor.Registro0.Aviso.New;
          FPagFor.Registro0.Aviso.Last.CodigoRetorno   := FPagFor.Lote.Last.SegmentoA.Last.CodOcorrencia;
          FPagFor.Registro0.Aviso.Last.MensagemRetorno := FPagFor.Lote.Last.SegmentoA.Last.DescOcorrencia;
          FPagFor.Registro0.Aviso.Last.Segmento        := 'A';
          FPagFor.Registro0.Aviso.Last.SegmentoFilho   := '';
          FPagFor.Registro0.Aviso.Last.SeuNumero       := FPagFor.Lote.Last.SegmentoA.Last.Credito.SeuNumero;
        end;
      end;

    pagBancoDoBrasil, pagBradesco:
      begin
        FPagFor.Lote.Last.SegmentoA.Last.Credito.ValorPagamento := StrToInt(Copy(FArquivoTXT.Strings[i], 120, 15)) / 100;
        FPagFor.Lote.Last.SegmentoA.Last.Credito.NossoNumero    := Copy(FArquivoTXT.Strings[i], 135, 20);
        FPagFor.Lote.Last.SegmentoA.Last.Credito.DataReal       := StringToDateTimeDef(Copy(FArquivoTXT.Strings[i], 155, 2)+'/'+Copy(FArquivoTXT.Strings[i], 157, 2)+'/'+Copy(FArquivoTXT.Strings[i], 159, 4), 0);
        FPagFor.Lote.Last.SegmentoA.Last.Credito.ValorReal      := StrToInt(Copy(FArquivoTXT.Strings[i], 163, 15)) / 100;
        FPagFor.Lote.Last.SegmentoA.Last.Informacao2            := Copy(FArquivoTXT.Strings[i], 178, 40);
        FPagFor.Lote.Last.SegmentoA.Last.CodigoDOC              := Copy(FArquivoTXT.Strings[i], 218, 2);
        FPagFor.Lote.Last.SegmentoA.Last.CodigoTED              := Copy(FArquivoTXT.Strings[i], 220, 5);
        FPagFor.Lote.Last.SegmentoA.Last.Aviso                  := StrToIntDef(Copy(FArquivoTXT.Strings[i], 230, 1),0);
        FPagFor.Lote.Last.SegmentoA.Last.CodOcorrencia          := Trim(Copy(FArquivoTXT.Strings[i], 231, 10));

        FPagFor.Lote.Last.SegmentoA.Last.DescOcorrencia := DescricaoRetornoBancoDoBrasil(FPagFor.Lote.Last.SegmentoA.Last.CodOcorrencia);

        if (Length(FPagFor.Lote.Last.SegmentoA.Last.CodOcorrencia) > 0) and (POS(FPagFor.Lote.Last.SegmentoA.Last.CodOcorrencia, PAGAMENTO_LIBERADO_AVISO) = 0) then
        begin
          FPagFor.Registro0.Aviso.New;
          FPagFor.Registro0.Aviso.Last.CodigoRetorno   := FPagFor.Lote.Last.SegmentoA.Last.CodOcorrencia;
          FPagFor.Registro0.Aviso.Last.MensagemRetorno := FPagFor.Lote.Last.SegmentoA.Last.DescOcorrencia;
          FPagFor.Registro0.Aviso.Last.Segmento        := 'A';
          FPagFor.Registro0.Aviso.Last.SegmentoFilho   := '';
          FPagFor.Registro0.Aviso.Last.SeuNumero       := FPagFor.Lote.Last.SegmentoA.Last.Credito.SeuNumero;
        end;
      end;
  end;

  while pos((Copy(FArquivoTXT.Strings[i+1], 8, 1) + Copy(FArquivoTXT.Strings[i+1], 14, 1)), '3B/3C/3D/3E/3F/3Z/') > 0 do
  begin
    inc(i); //próxima linha do txt a ser lida
    {opcionais do segmento A}
    LerSegmentoB(FPagFor.Lote.Last.SegmentoA.Last.SegmentoB, i);
    LerSegmentoC(FPagFor.Lote.Last.SegmentoA.Last.SegmentoC, i);
    LerSegmentoD(FPagFor.Lote.Last.SegmentoA.Last.SegmentoD, i);
    LerSegmentoE(FPagFor.Lote.Last.SegmentoA.Last.SegmentoE, i);
    LerSegmentoF(FPagFor.Lote.Last.SegmentoA.Last.SegmentoF, i);
    LerSegmentoZ(FPagFor.Lote.Last.SegmentoA.Last.SegmentoZ, i);

    case FPagFor.Geral.Banco of
      pagItau:
        begin
          for x := 0 to FPagFor.Lote.Last.SegmentoA.Last.SegmentoB.Count - 1 do
          begin
            if POS(FPagFor.Lote.Last.SegmentoA.Last.SegmentoB.Items[x].CodOcorrencia, PAGAMENTO_LIBERADO_AVISO) = 0 then
            begin
              FPagFor.Registro0.Aviso.New;
              FPagFor.Registro0.Aviso.Last.CodigoRetorno   := FPagFor.Lote.Last.SegmentoA.Last.SegmentoB.Last.CodOcorrencia;
              FPagFor.Registro0.Aviso.Last.MensagemRetorno := FPagFor.Lote.Last.SegmentoA.Last.SegmentoB.Items[x].DescOcorrencia;
              FPagFor.Registro0.Aviso.Last.Segmento        := 'A';
              FPagFor.Registro0.Aviso.Last.SegmentoFilho   := 'B';
              FPagFor.Registro0.Aviso.Last.SeuNumero       := FPagFor.Lote.Last.SegmentoA.Last.Credito.SeuNumero;
            end;
          end;

          for x := 0 to FPagFor.Lote.Last.SegmentoA.Last.SegmentoC.Count - 1 do
          begin
            if POS(FPagFor.Lote.Last.SegmentoA.Last.SegmentoC.Items[x].CodOcorrencia, PAGAMENTO_LIBERADO_AVISO) = 0 then
            begin
              FPagFor.Registro0.Aviso.New;
              FPagFor.Registro0.Aviso.Last.CodigoRetorno   := FPagFor.Lote.Last.SegmentoA.Last.SegmentoC.Last.CodOcorrencia;
              FPagFor.Registro0.Aviso.Last.MensagemRetorno := FPagFor.Lote.Last.SegmentoA.Last.SegmentoC.Items[x].DescOcorrencia;
              FPagFor.Registro0.Aviso.Last.Segmento        := 'A';
              FPagFor.Registro0.Aviso.Last.SegmentoFilho   := 'C';
              FPagFor.Registro0.Aviso.Last.SeuNumero       := FPagFor.Lote.Last.SegmentoA.Last.Credito.SeuNumero;
            end;
          end;

          for x := 0 to FPagFor.Lote.Last.SegmentoA.Last.SegmentoD.Count - 1 do
          begin
            if POS(FPagFor.Lote.Last.SegmentoA.Last.SegmentoD.Items[x].CodOcorrencia, PAGAMENTO_LIBERADO_AVISO) = 0 then
            begin
              FPagFor.Registro0.Aviso.New;
              FPagFor.Registro0.Aviso.Last.CodigoRetorno   := FPagFor.Lote.Last.SegmentoA.Last.SegmentoD.Last.CodOcorrencia;
              FPagFor.Registro0.Aviso.Last.MensagemRetorno := FPagFor.Lote.Last.SegmentoA.Last.SegmentoD.Items[x].DescOcorrencia;
              FPagFor.Registro0.Aviso.Last.Segmento        := 'A';
              FPagFor.Registro0.Aviso.Last.SegmentoFilho   := 'D';
              FPagFor.Registro0.Aviso.Last.SeuNumero       := FPagFor.Lote.Last.SegmentoA.Last.Credito.SeuNumero;
            end;
          end;

          for x := 0 to FPagFor.Lote.Last.SegmentoA.Last.SegmentoE.Count - 1 do
          begin
            if POS(FPagFor.Lote.Last.SegmentoA.Last.SegmentoE.Items[x].CodOcorrencia, PAGAMENTO_LIBERADO_AVISO) = 0 then
            begin
              FPagFor.Registro0.Aviso.New;
              FPagFor.Registro0.Aviso.Last.CodigoRetorno   := FPagFor.Lote.Last.SegmentoA.Last.SegmentoE.Last.CodOcorrencia;
              FPagFor.Registro0.Aviso.Last.MensagemRetorno := FPagFor.Lote.Last.SegmentoA.Last.SegmentoE.Items[x].DescOcorrencia;
              FPagFor.Registro0.Aviso.Last.Segmento        := 'A';
              FPagFor.Registro0.Aviso.Last.SegmentoFilho   := 'E';
              FPagFor.Registro0.Aviso.Last.SeuNumero       := FPagFor.Lote.Last.SegmentoA.Last.Credito.SeuNumero;
            end;
          end;

          for x := 0 to FPagFor.Lote.Last.SegmentoA.Last.SegmentoF.Count - 1 do
          begin
            if POS(FPagFor.Lote.Last.SegmentoA.Last.SegmentoF.Items[x].CodOcorrencia, PAGAMENTO_LIBERADO_AVISO) = 0 then
            begin
              FPagFor.Registro0.Aviso.New;
              FPagFor.Registro0.Aviso.Last.CodigoRetorno   := FPagFor.Lote.Last.SegmentoA.Last.SegmentoF.Last.CodOcorrencia;
              FPagFor.Registro0.Aviso.Last.MensagemRetorno := FPagFor.Lote.Last.SegmentoA.Last.SegmentoF.Items[x].DescOcorrencia;
              FPagFor.Registro0.Aviso.Last.Segmento        := 'A';
              FPagFor.Registro0.Aviso.Last.SegmentoFilho   := 'F';
              FPagFor.Registro0.Aviso.Last.SeuNumero       := FPagFor.Lote.Last.SegmentoA.Last.Credito.SeuNumero;
            end;
          end;
        end;
    end;
  end;
end;

procedure TPagForR.LerSegmentoB(mSegmentoBList: TSegmentoBList; I:Integer);
var
  mOk:boolean;
begin
  if ((Copy(FArquivoTXT.Strings[i], 8, 1) + Copy(FArquivoTXT.Strings[i], 14, 1)) <> '3B') then
    Exit;

  if Trim(Copy(FArquivoTXT.Strings[i], 15, 2)) <> '' then    // PIX
  begin
    case FPagFor.Geral.Banco of
      pagSicredi:
        begin
          mSegmentoBList.New;
          mSegmentoBList.Last.Inscricao.Tipo   := StrToTpInscricao(mOk,Copy(FArquivoTXT.Strings[i], 18, 1));
          mSegmentoBList.Last.Inscricao.Numero := Copy(FArquivoTXT.Strings[i], 19, 14);
          mSegmentoBList.Last.PixTipoChave     := StrToTipoChavePIX(mok,copy(FArquivoTXT.Strings[i], 15, 2));
          mSegmentoBList.Last.PixMensagem      := Copy(FArquivoTXT.Strings[i], 128, 99);
          mSegmentoBList.Last.CodigoUG         := StrToIntDef(Copy(FArquivoTXT.Strings[i], 227, 6),0);
          mSegmentoBList.Last.CodigoISPB       := StrToIntDef(Copy(FArquivoTXT.Strings[i], 233, 8),0);
        end;
    end;
  end
  else
  begin
    mSegmentoBList.New;
    mSegmentoBList.Last.Inscricao.Tipo       := StrToTpInscricao(mOk, Copy(FArquivoTXT.Strings[i], 18, 1));
    mSegmentoBList.Last.Inscricao.Numero     := Copy(FArquivoTXT.Strings[i], 19, 14);
    mSegmentoBList.Last.Endereco.Logradouro  := Copy(FArquivoTXT.Strings[i], 33, 30);
    mSegmentoBList.Last.Endereco.Numero      := StrToIntDef(Copy(FArquivoTXT.Strings[i], 63, 5),0);
    mSegmentoBList.Last.Endereco.Complemento := Copy(FArquivoTXT.Strings[i], 68, 15);
    mSegmentoBList.Last.Endereco.Bairro      := Copy(FArquivoTXT.Strings[i], 83, 15);
    mSegmentoBList.Last.Endereco.Cidade      := Copy(FArquivoTXT.Strings[i], 98, 20);
    mSegmentoBList.Last.Endereco.CEP         := StrToIntDef(Copy(FArquivoTXT.Strings[i], 118, 8),0);
    mSegmentoBList.Last.Endereco.Estado      := Copy(FArquivoTXT.Strings[i], 126, 2);

    case FPagFor.Geral.Banco of
      pagItau:
        begin
          mSegmentoBList.Last.Email := Copy(FArquivoTXT.Strings[i], 128, 100);
        end;

      pagSicredi:
        begin
          mSegmentoBList.Last.Valor      := StrToInt(Copy(FArquivoTXT.Strings[i], 136, 13)) / 100;
          mSegmentoBList.Last.Abatimento := StrToInt(Copy(FArquivoTXT.Strings[i], 151, 13)) / 100;
          mSegmentoBList.Last.Desconto   := StrToInt(Copy(FArquivoTXT.Strings[i], 166, 13)) / 100;
          mSegmentoBList.Last.Mora       := StrToInt(Copy(FArquivoTXT.Strings[i], 181, 13)) / 100;
          mSegmentoBList.Last.Multa      := StrToInt(Copy(FArquivoTXT.Strings[i], 196, 13)) / 100;

          // PIX
          if Trim(Copy(FArquivoTXT.Strings[i], 15, 2)) <> '' then
          begin
            mSegmentoBList.Last.PixTipoChave := StrToTipoChavePix(mOk, Copy(FArquivoTXT.Strings[i], 15, 2));
            mSegmentoBList.Last.PixMensagem  := Copy(FArquivoTXT.Strings[i], 128, 99);
            mSegmentoBList.Last.CodigoUG     := StrToInt(Copy(FArquivoTXT.Strings[i], 227, 6));
            mSegmentoBList.Last.CodigoISPB   := StrToInt(Copy(FArquivoTXT.Strings[i], 233, 8));
          end;
        end;
    end;
  end;
end;

procedure TPagForR.LerSegmentoC(mSegmentoCList: TSegmentoCList; I:Integer);
begin
  if ((Copy(FArquivoTXT.Strings[i], 8, 1) + Copy(FArquivoTXT.Strings[i], 14, 1)) <> '3C') then
    Exit;

  mSegmentoCList.New;
  case FPagFor.Geral.Banco of
    pagItau:
      begin
        mSegmentoCList.Last.ValorCSLL             := StrToInt(Copy(FArquivoTXT.Strings[i], 15, 15)) / 100;
        mSegmentoCList.Last.Vencimento            := StringToDateTime(Copy(FArquivoTXT.Strings[i], 38, 2)+'/'+Copy(FArquivoTXT.Strings[i], 40, 2)+'/'+Copy(FArquivoTXT.Strings[i], 42, 4));
        mSegmentoCList.Last.ValorDocumento        := StrToInt(Copy(FArquivoTXT.Strings[i], 46, 15)) / 100;
        mSegmentoCList.Last.ValorPIS              := StrToInt(Copy(FArquivoTXT.Strings[i], 61, 15)) / 100;
        mSegmentoCList.Last.ValorIR               := StrToInt(Copy(FArquivoTXT.Strings[i], 76, 15)) / 100;
        mSegmentoCList.Last.ValorISS              := StrToInt(Copy(FArquivoTXT.Strings[i], 91, 15)) / 100;
        mSegmentoCList.Last.ValorCOFINS           := StrToInt(Copy(FArquivoTXT.Strings[i], 106, 15)) / 100;
        mSegmentoCList.Last.Descontos             := StrToInt(Copy(FArquivoTXT.Strings[i], 121, 15)) / 100;
        mSegmentoCList.Last.Abatimentos           := StrToInt(Copy(FArquivoTXT.Strings[i], 136, 15)) / 100;
        mSegmentoCList.Last.Deducoes              := StrToInt(Copy(FArquivoTXT.Strings[i], 151, 15)) / 100;
        mSegmentoCList.Last.Mora                  := StrToInt(Copy(FArquivoTXT.Strings[i], 166, 15)) / 100;
        mSegmentoCList.Last.Multa                 := StrToInt(Copy(FArquivoTXT.Strings[i], 181, 15)) / 100;
        mSegmentoCList.Last.Acrescimos            := StrToInt(Copy(FArquivoTXT.Strings[i], 196, 15)) / 100;
        mSegmentoCList.Last.NumeroFaturaDocumento := Copy(FArquivoTXT.Strings[i], 211, 20);
      end;
  end;
end;

procedure TPagForR.LerSegmentoD(mSegmentoDList: TSegmentoDList; I:Integer);
begin
  if ((Copy(FArquivoTXT.Strings[i], 8, 1) + Copy(FArquivoTXT.Strings[i], 14, 1)) <> '3D') then
    Exit;

  mSegmentoDList.New;
  case FPagFor.Geral.Banco of
    pagItau:
      begin
        mSegmentoDList.Last.PeriodoCompetencia        := StrToInt(Copy(FArquivoTXT.Strings[i], 18, 6));
        mSegmentoDList.Last.CentroCusto               := Copy(FArquivoTXT.Strings[i], 24, 15);
        mSegmentoDList.Last.CodigoFuncionario         := Copy(FArquivoTXT.Strings[i], 39, 15);
        mSegmentoDList.Last.Cargo                     := Copy(FArquivoTXT.Strings[i], 54, 30);
        mSegmentoDList.Last.FeriasInicio              := StringToDateTime(Copy(FArquivoTXT.Strings[i], 84, 2)+'/'+Copy(FArquivoTXT.Strings[i], 86, 2)+'/'+Copy(FArquivoTXT.Strings[i], 88, 4));
        mSegmentoDList.Last.FeriasFim                 := StringToDateTime(Copy(FArquivoTXT.Strings[i], 92, 2)+'/'+Copy(FArquivoTXT.Strings[i], 94, 2)+'/'+Copy(FArquivoTXT.Strings[i], 96, 4));
        mSegmentoDList.Last.DependentesIR             := StrToInt(Copy(FArquivoTXT.Strings[i], 100, 2));
        mSegmentoDList.Last.DependentesSalarioFamilia := StrToInt(Copy(FArquivoTXT.Strings[i], 102, 2));
        mSegmentoDList.Last.HorasSemanais             := StrToInt(Copy(FArquivoTXT.Strings[i], 104, 2));
        mSegmentoDList.Last.SalarioContribuicao       := StrToInt(Copy(FArquivoTXT.Strings[i], 106, 15)) / 100;
        mSegmentoDList.Last.FGTS                      := StrToInt(Copy(FArquivoTXT.Strings[i], 121, 15)) / 100;
        mSegmentoDList.Last.ValorCredito              := StrToInt(Copy(FArquivoTXT.Strings[i], 136, 15)) / 100;
        mSegmentoDList.Last.ValorDebito               := StrToInt(Copy(FArquivoTXT.Strings[i], 151, 15)) / 100;
        mSegmentoDList.Last.ValorLiquido              := StrToInt(Copy(FArquivoTXT.Strings[i], 166, 15)) / 100;
        mSegmentoDList.Last.ValorBase                 := StrToInt(Copy(FArquivoTXT.Strings[i], 181, 15)) / 100;
        mSegmentoDList.Last.BaseCalculoIRRF           := StrToInt(Copy(FArquivoTXT.Strings[i], 196, 15)) / 100;
        mSegmentoDList.Last.BaseCalculoFGTS           := StrToInt(Copy(FArquivoTXT.Strings[i], 211, 15)) / 100;
        mSegmentoDList.Last.Disponibilizacao          := Copy(FArquivoTXT.Strings[i], 226, 2);
        mSegmentoDList.Last.CodOcorrencia             := Trim(Copy(FArquivoTXT.Strings[i], 231, 10));
        mSegmentoDList.Last.DescOcorrencia            := DescricaoRetornoItau(mSegmentoDList.Last.CodOcorrencia);
      end;
  end;
end;

procedure TPagForR.LerSegmentoE(mSegmentoEList: TSegmentoEList; I:Integer);
var
  mOk:boolean;
begin
  if ((Copy(FArquivoTXT.Strings[i], 8, 1) + Copy(FArquivoTXT.Strings[i], 14, 1)) <> '3E') then
    Exit;

  mSegmentoEList.New;
  mSegmentoEList.Last.Movimento              := StrToTpMovimentoPagto(mOk, Copy(FArquivoTXT.Strings[i], 18, 1));
  mSegmentoEList.Last.InformacaoComplementar := Copy(FArquivoTXT.Strings[i], 19, 200);
  mSegmentoEList.Last.CodOcorrencia          := Trim(Copy(FArquivoTXT.Strings[i], 231, 10));
  mSegmentoEList.Last.DescOcorrencia         := DescricaoRetornoItau(mSegmentoEList.Last.CodOcorrencia);
end;

procedure TPagForR.LerSegmentoF(mSegmentoFList: TSegmentoFList; I:Integer);
begin
  if ((Copy(FArquivoTXT.Strings[i], 8, 1) + Copy(FArquivoTXT.Strings[i], 14, 1)) <> '3F') then
    Exit;

  mSegmentoFList.New;
  mSegmentoFList.Last.InformacaoComplementar := Copy(FArquivoTXT.Strings[i], 18, 144);
  mSegmentoFList.Last.CodOcorrencia          := Trim(Copy(FArquivoTXT.Strings[i], 231, 10));
  mSegmentoFList.Last.DescOcorrencia         := DescricaoRetornoItau(mSegmentoFList.Last.CodOcorrencia);
end;

procedure TPagForR.LerSegmentoG(I: Integer);
var
  mOk: Boolean;
begin
  if (Copy(FArquivoTXT.Strings[i], 8, 1) + Copy(FArquivoTXT.Strings[i], 14, 1) <> '3G') then
    Exit;

  FPagFor.Lote.Last.SegmentoG.New;
  FPagFor.Lote.Last.SegmentoG.Last.CodigoBarras           := Copy(FArquivoTXT.Strings[i], 18, 44);
  FPagFor.Lote.Last.SegmentoG.Last.Cedente.Inscricao.Tipo := StrToTpInscricao(mOk, Copy(FArquivoTXT.Strings[i], 62, 1));

  if FPagFor.Lote.Last.SegmentoG.Last.Cedente.Inscricao.Tipo = tiCNPJ then
    FPagFor.Lote.Last.SegmentoG.Last.Cedente.Inscricao.Numero := Copy(FArquivoTXT.Strings[i], 64, 14)
  else
    FPagFor.Lote.Last.SegmentoG.Last.Cedente.Inscricao.Numero := Copy(FArquivoTXT.Strings[i], 67, 11);

  FPagFor.Lote.Last.SegmentoG.Last.Cedente.Nome     := Trim(Copy(FArquivoTXT.Strings[i], 78, 30));

  //Sicredi envia a data no formato AAAAMMDD
  case FPagFor.Geral.Banco of
      pagSicredi:
      begin
         FPagFor.Lote.Last.SegmentoG.Last.Vencimento       := StringToDateTime(Copy(FArquivoTXT.Strings[i], 114, 2) + '/' + Copy(FArquivoTXT.Strings[i], 112, 2) + '/' + Copy(FArquivoTXT.Strings[i], 108, 4));
         FPagFor.Lote.Last.SegmentoG.Last.DataEmissao      := StringToDateTime(Copy(FArquivoTXT.Strings[i], 188, 2) + '/' + Copy(FArquivoTXT.Strings[i], 186, 2) + '/' + Copy(FArquivoTXT.Strings[i], 182, 4));
         FPagFor.Lote.Last.SegmentoG.Last.Desconto1.Data   := StringToDateTime(Copy(FArquivoTXT.Strings[i], 188, 2) + '/' + Copy(FArquivoTXT.Strings[i], 186, 2) + '/' + Copy(FArquivoTXT.Strings[i], 182, 4));
         FPagFor.Lote.Last.SegmentoG.Last.DataLimite       := StringToDateTime(Copy(FArquivoTXT.Strings[i], 238, 2) + '/' + Copy(FArquivoTXT.Strings[i], 236, 2) + '/' + Copy(FArquivoTXT.Strings[i], 232, 4));
      end;
  else
      begin
         FPagFor.Lote.Last.SegmentoG.Last.Vencimento       := StringToDateTime(Copy(FArquivoTXT.Strings[i], 108, 2) + '/' + Copy(FArquivoTXT.Strings[i], 110, 2) + '/' + Copy(FArquivoTXT.Strings[i], 112, 4));
         FPagFor.Lote.Last.SegmentoG.Last.DataEmissao      := StringToDateTime(Copy(FArquivoTXT.Strings[i], 182, 2) + '/' + Copy(FArquivoTXT.Strings[i], 184, 2) + '/' + Copy(FArquivoTXT.Strings[i], 186, 4));
         FPagFor.Lote.Last.SegmentoG.Last.Desconto1.Data   := StringToDateTime(Copy(FArquivoTXT.Strings[i], 182, 2) + '/' + Copy(FArquivoTXT.Strings[i], 184, 2) + '/' + Copy(FArquivoTXT.Strings[i], 186, 4));
         FPagFor.Lote.Last.SegmentoG.Last.DataLimite       := StringToDateTime(Copy(FArquivoTXT.Strings[i], 232, 2) + '/' + Copy(FArquivoTXT.Strings[i], 234, 2) + '/' + Copy(FArquivoTXT.Strings[i], 236, 4));
      end;
  end;
  FPagFor.Lote.Last.SegmentoG.Last.ValorTitulo      := StrToFloat(Copy(FArquivoTXT.Strings[i], 116, 13) + ',' + Copy(FArquivoTXT.Strings[i], 129, 2));
  FPagFor.Lote.Last.SegmentoG.Last.QtdeMoeda        := StrToFloat(Copy(FArquivoTXT.Strings[i], 131, 10) + ',' + Copy(FArquivoTXT.Strings[i], 141, 5));
  FPagFor.Lote.Last.SegmentoG.Last.CodigoMoeda      := StrToInt(Copy(FArquivoTXT.Strings[i], 146, 2));
  FPagFor.Lote.Last.SegmentoG.Last.NumeroDocumento  := Trim(Copy(FArquivoTXT.Strings[i], 148, 15));
  FPagFor.Lote.Last.SegmentoG.Last.AgenciaCobradora := StrToInt(Copy(FArquivoTXT.Strings[i], 163, 5));
  FPagFor.Lote.Last.SegmentoG.Last.DVCobradora      := Copy(FArquivoTXT.Strings[i], 168, 1);
  FPagFor.Lote.Last.SegmentoG.Last.Praca            := Copy(FArquivoTXT.Strings[i], 169, 10);
  FPagFor.Lote.Last.SegmentoG.Last.Carteira         := Copy(FArquivoTXT.Strings[i], 179, 1);
  FPagFor.Lote.Last.SegmentoG.Last.EspecieTitulo    := StrToInt(Copy(FArquivoTXT.Strings[i], 180, 2));
  FPagFor.Lote.Last.SegmentoG.Last.JurosMora        := StrToFloat(Copy(FArquivoTXT.Strings[i], 190, 13) + ',' + Copy(FArquivoTXT.Strings[i], 203, 2));

  //Em algumas situações o banco manda tudo como 999... ou 555...
  //Multa não pode ultrapassar 10%
  if FPagFor.Lote.Last.SegmentoG.Last.JurosMora > (FPagFor.Lote.Last.SegmentoG.Last.ValorTitulo * 0.1) then
    FPagFor.Lote.Last.SegmentoG.Last.JurosMora := 0;

  FPagFor.Lote.Last.SegmentoG.Last.Desconto1.Codigo := StrToIntDef(Trim(Copy(FArquivoTXT.Strings[i], 205, 1)),0);
  FPagFor.Lote.Last.SegmentoG.Last.Desconto1.Valor  := StrToFloat(Copy(FArquivoTXT.Strings[i], 214, 13) + ',' + Copy(FArquivoTXT.Strings[i], 227, 2));

  FPagFor.Lote.Last.SegmentoG.Last.CodigoProtesto   := StrToInt(Copy(FArquivoTXT.Strings[i], 229, 1));
  FPagFor.Lote.Last.SegmentoG.Last.PrazoProtesto    := StrToInt(Copy(FArquivoTXT.Strings[i], 230, 2));

end;

procedure TPagForR.LerSegmentoH(mSegmentoHList: TSegmentoHList; I: Integer);
var
  mOk: Boolean;
begin
  if (Copy(FArquivoTXT.Strings[i], 8, 1) + Copy(FArquivoTXT.Strings[i], 14, 1) <> '3H') then
    Exit;

  mSegmentoHList.New;
  mSegmentoHList.Last.Avalista.Inscricao.Tipo := StrToTpInscricao(mOk, Copy(FArquivoTXT.Strings[i], 18, 1));

  if mSegmentoHList.Last.Avalista.Inscricao.Tipo = tiCNPJ then
    mSegmentoHList.Last.Avalista.Inscricao.Numero := Copy(FArquivoTXT.Strings[i], 20, 14)
  else
    mSegmentoHList.Last.Avalista.Inscricao.Numero := Copy(FArquivoTXT.Strings[i], 23, 11);

  mSegmentoHList.Last.Avalista.Nome := Trim(Copy(FArquivoTXT.Strings[i], 34, 40));

  //Sicredi envia a data no formato AAAAMMDD
  case FPagFor.Geral.Banco of
      pagSicredi:
      begin
           mSegmentoHList.Last.Desconto2.Codigo := StrToIntDef(Trim(Copy(FArquivoTXT.Strings[i], 74, 1)),0);

           if Copy(FArquivoTXT.Strings[i], 75, 8) <> '00000000' then
             mSegmentoHList.Last.Desconto2.Data := StringToDateTime(Copy(FArquivoTXT.Strings[i], 81, 2) + '/' + Copy(FArquivoTXT.Strings[i], 79, 2) + '/' + Copy(FArquivoTXT.Strings[i], 75, 4));

           mSegmentoHList.Last.Desconto2.Valor := StrToFloat(Copy(FArquivoTXT.Strings[i], 83, 13) + ',' + Copy(FArquivoTXT.Strings[i], 96, 2));

           mSegmentoHList.Last.Desconto3.Codigo := StrToIntDef(Trim(Copy(FArquivoTXT.Strings[i], 98, 1)),0);

           if Copy(FArquivoTXT.Strings[i], 99, 8) <> '00000000' then
             mSegmentoHList.Last.Desconto3.Data := StringToDateTime(Copy(FArquivoTXT.Strings[i], 105, 2) + '/' + Copy(FArquivoTXT.Strings[i], 103, 2) + '/' + Copy(FArquivoTXT.Strings[i], 99, 4));

           mSegmentoHList.Last.Desconto3.Valor := StrToFloat(Copy(FArquivoTXT.Strings[i], 107, 13) + ',' + Copy(FArquivoTXT.Strings[i], 120, 2));

           mSegmentoHList.Last.Multa.Codigo := StrToIntDef(Trim(Copy(FArquivoTXT.Strings[i], 122, 1)),0);

           if Copy(FArquivoTXT.Strings[i], 123, 8) <> '00000000' then
             mSegmentoHList.Last.Multa.Data := StringToDateTime(Copy(FArquivoTXT.Strings[i], 129, 2) + '/' + Copy(FArquivoTXT.Strings[i], 127, 2) + '/' + Copy(FArquivoTXT.Strings[i], 123, 4));

           mSegmentoHList.Last.Multa.Valor := StrToFloat(Copy(FArquivoTXT.Strings[i], 131, 13) + ',' + Copy(FArquivoTXT.Strings[i], 144, 2));

      end;
  else
      begin
           mSegmentoHList.Last.Desconto2.Codigo := StrToIntDef(Trim(Copy(FArquivoTXT.Strings[i], 74, 1)),0);

           if Copy(FArquivoTXT.Strings[i], 75, 8) <> '00000000' then
             mSegmentoHList.Last.Desconto2.Data := StringToDateTime(Copy(FArquivoTXT.Strings[i], 75, 2) + '/' + Copy(FArquivoTXT.Strings[i], 77, 2) + '/' + Copy(FArquivoTXT.Strings[i], 79, 4));

           mSegmentoHList.Last.Desconto2.Valor := StrToFloat(Copy(FArquivoTXT.Strings[i], 83, 13) + ',' + Copy(FArquivoTXT.Strings[i], 96, 2));

           mSegmentoHList.Last.Desconto3.Codigo := StrToIntDef(Trim(Copy(FArquivoTXT.Strings[i], 98, 1)),0);

           if Copy(FArquivoTXT.Strings[i], 99, 8) <> '00000000' then
             mSegmentoHList.Last.Desconto3.Data := StringToDateTime(Copy(FArquivoTXT.Strings[i], 99, 2) + '/' + Copy(FArquivoTXT.Strings[i], 101, 2) + '/' + Copy(FArquivoTXT.Strings[i], 103, 4));

           mSegmentoHList.Last.Desconto3.Valor := StrToFloat(Copy(FArquivoTXT.Strings[i], 107, 13) + ',' + Copy(FArquivoTXT.Strings[i], 120, 2));

           mSegmentoHList.Last.Multa.Codigo := StrToIntDef(Trim(Copy(FArquivoTXT.Strings[i], 122, 1)),0);

           if Copy(FArquivoTXT.Strings[i], 123, 8) <> '00000000' then
             mSegmentoHList.Last.Multa.Data := StringToDateTime(Copy(FArquivoTXT.Strings[i], 123, 2) + '/' + Copy(FArquivoTXT.Strings[i], 125, 2) + '/' + Copy(FArquivoTXT.Strings[i], 127, 4));

           mSegmentoHList.Last.Multa.Valor := StrToFloat(Copy(FArquivoTXT.Strings[i], 131, 13) + ',' + Copy(FArquivoTXT.Strings[i], 144, 2));

      end;

  end;


  mSegmentoHList.Last.Abatimento  := StrToFloat(Copy(FArquivoTXT.Strings[i], 146, 13) + ',' + Copy(FArquivoTXT.Strings[i], 159, 2));
  mSegmentoHList.Last.Informacao1 := Trim(Copy(FArquivoTXT.Strings[i], 161, 40));
  mSegmentoHList.Last.Informacao2 := Trim(Copy(FArquivoTXT.Strings[i], 201, 40));
end;

procedure TPagForR.LerSegmentoJ(I: Integer; var LeuRegistroJ: boolean);
var
  mOk:boolean;
  x:integer;
begin
  if ((Copy(FArquivoTXT.Strings[i], 8, 1) + Copy(FArquivoTXT.Strings[i], 14, 1)) <> '3J') and
     ((Copy(FArquivoTXT.Strings[i], 8, 1) + Copy(FArquivoTXT.Strings[i], 14, 1)) <> '3B') and
     ((Copy(FArquivoTXT.Strings[i], 8, 1) + Copy(FArquivoTXT.Strings[i], 14, 1)) <> '3C') and
     ((Copy(FArquivoTXT.Strings[i], 8, 1) + Copy(FArquivoTXT.Strings[i], 14, 1)) <> '3Z') then
    Exit;

  if (FPagFor.Geral.Banco = pagBradesco) and (Copy(FArquivoTXT.Strings[i], 18, 2) = '52') then
    Exit;

  if ((Copy(FArquivoTXT.Strings[i], 8, 1) + Copy(FArquivoTXT.Strings[i], 14, 1)) = '3J') then
    LeuRegistroJ := True;

  if ((Copy(FArquivoTXT.Strings[i], 18, 2) <> '52') and (Copy(FArquivoTXT.Strings[i], 18, 2) <> '99')) and
     ((Copy(FArquivoTXT.Strings[i], 8, 1) + Copy(FArquivoTXT.Strings[i], 14, 1)) <> '3B') and
     ((Copy(FArquivoTXT.Strings[i], 8, 1) + Copy(FArquivoTXT.Strings[i], 14, 1)) <> '3C') and
     ((Copy(FArquivoTXT.Strings[i], 8, 1) + Copy(FArquivoTXT.Strings[i], 14, 1)) <> '3Z') then
  begin
    FPagFor.Lote.Last.SegmentoJ.New;
    FPagFor.Lote.Last.SegmentoJ.Last.CodMovimento   := StrToInMovimento(mOk, Copy(FArquivoTXT.Strings[i], 15, 3));
    FPagFor.Lote.Last.SegmentoJ.Last.CodigoBarras   := Copy(FArquivoTXT.Strings[i], 18, 44);
    FPagFor.Lote.Last.SegmentoJ.Last.NomeCedente    := Copy(FArquivoTXT.Strings[i], 62, 30);
    FPagFor.Lote.Last.SegmentoJ.Last.DataVencimento := StringToDateTime(Copy(FArquivoTXT.Strings[i], 92, 2)+'/'+Copy(FArquivoTXT.Strings[i], 94, 2)+'/'+Copy(FArquivoTXT.Strings[i], 96, 4));

    case FPagFor.Geral.Banco of
      pagItau,
      pagSantander,
      pagSicredi,
      pagBancoCECRED,
      pagBradesco,
      pagBancoDoBrasil:
        begin
          FPagFor.Lote.Last.SegmentoJ.Last.ValorTitulo      := StrToInt(Copy(FArquivoTXT.Strings[i], 100, 15)) / 100;
          FPagFor.Lote.Last.SegmentoJ.Last.Desconto         := StrToInt(Copy(FArquivoTXT.Strings[i], 115, 15)) / 100;
          FPagFor.Lote.Last.SegmentoJ.Last.Acrescimo        := StrToInt(Copy(FArquivoTXT.Strings[i], 130, 15)) / 100;
          FPagFor.Lote.Last.SegmentoJ.Last.DataPagamento    := StringToDateTime(Copy(FArquivoTXT.Strings[i], 145, 2)+'/'+Copy(FArquivoTXT.Strings[i], 147, 2)+'/'+Copy(FArquivoTXT.Strings[i], 149, 4));
          FPagFor.Lote.Last.SegmentoJ.Last.ValorPagamento   := StrToInt(Copy(FArquivoTXT.Strings[i], 153, 15)) / 100;
          FPagFor.Lote.Last.SegmentoJ.Last.QtdeMoeda        := StrToInt(Copy(FArquivoTXT.Strings[i], 168, 15)) / 100000;
          FPagFor.Lote.Last.SegmentoJ.Last.ReferenciaSacado := Copy(FArquivoTXT.Strings[i], 183, 20);

          case FPagFor.Geral.Banco of
            pagSicredi,
            pagBancoCECRED,
            pagBradesco,
            pagBancoDoBrasil:
              FPagFor.Lote.Last.SegmentoJ.Last.NossoNumero := Copy(FArquivoTXT.Strings[i], 203, 20);
          else
            FPagFor.Lote.Last.SegmentoJ.Last.NossoNumero := Copy(FArquivoTXT.Strings[i], 216, 15)
          end;

          FPagFor.Lote.Last.SegmentoJ.Last.CodOcorrencia := Trim(Copy(FArquivoTXT.Strings[i], 231, 10));

          case FPagFor.Geral.Banco of
            pagItau:
              FPagFor.Lote.Last.SegmentoJ.Last.DescOcorrencia := DescricaoRetornoItau(FPagFor.Lote.Last.SegmentoJ.Last.CodOcorrencia);

            pagBancoDoBrasil:
              FPagFor.Lote.Last.SegmentoJ.Last.DescOcorrencia := DescricaoRetornoBancoDoBrasil(FPagFor.Lote.Last.SegmentoJ.Last.CodOcorrencia);
          end;
        end;
    end;

    if POS(FPagFor.Lote.Last.SegmentoJ.Last.CodOcorrencia, PAGAMENTO_LIBERADO_AVISO) = 0 then
    begin
      FPagFor.Registro0.Aviso.New;
      FPagFor.Registro0.Aviso.Last.CodigoRetorno   := FPagFor.Lote.Last.SegmentoJ.Last.CodOcorrencia;
      FPagFor.Registro0.Aviso.Last.MensagemRetorno := FPagFor.Lote.Last.SegmentoJ.Last.DescOcorrencia;
      FPagFor.Registro0.Aviso.Last.Segmento        := 'J';
      FPagFor.Registro0.Aviso.Last.SegmentoFilho   := '';
      FPagFor.Registro0.Aviso.Last.SeuNumero       := FPagFor.Lote.Last.SegmentoJ.Last.ReferenciaSacado;
    end;
  end;

  // Segmentos B, C, Z, etc. também existem para outros tipos de segmento que não sejam o J, portanto,
  // só deve processar nessa rotina se o lote que está sendo processado é realmente de tipos J.
  // O Itau, por exemplo, retorna arquivo com segmentos A contendo segmentos B quando é pagamento de PIX
  // e nesse caso, não pode processar o segmento B nessa rotina pois não se refere a segmentos J.
  if not LeuRegistroJ then
    exit;

  while (pos((Copy(FArquivoTXT.Strings[i+1], 8, 1) + Copy(FArquivoTXT.Strings[i+1], 14, 1)), '3B/3C/3D/3E/3F/3Z/') > 0) or
        ((Copy(FArquivoTXT.Strings[i+1], 8, 1) + Copy(FArquivoTXT.Strings[i+1], 14, 1)) = '3J') and
        (pos(Copy(FArquivoTXT.Strings[i+1], 18, 2),'52/99/') > 0) do
  begin
    inc(i); //próxima linha do txt a ser lida

    {opcionais segmento J}
    LerSegmentoJ52(FPagFor.Lote.Last.SegmentoJ.Last.SegmentoJ52, i);
    LerSegmentoJ99(FPagFor.Lote.Last.SegmentoJ.Last.SegmentoJ99, i);
    LerSegmentoB(FPagFor.Lote.Last.SegmentoJ.Last.SegmentoB, i);
    LerSegmentoC(FPagFor.Lote.Last.SegmentoJ.Last.SegmentoC, i);
    LerSegmentoZ(FPagFor.Lote.Last.SegmentoJ.Last.SegmentoZ, i);

    case FPagFor.Geral.Banco of
      pagItau:
        begin
          for x := 0 to FPagFor.Lote.Last.SegmentoJ.Last.SegmentoB.Count - 1 do
          begin
            if POS(FPagFor.Lote.Last.SegmentoJ.Last.SegmentoB.Items[x].CodOcorrencia, PAGAMENTO_LIBERADO_AVISO) = 0 then
            begin
              FPagFor.Registro0.Aviso.New;
              FPagFor.Registro0.Aviso.Last.CodigoRetorno   := FPagFor.Lote.Last.SegmentoJ.Last.SegmentoB.Items[x].CodOcorrencia;
              FPagFor.Registro0.Aviso.Last.MensagemRetorno := FPagFor.Lote.Last.SegmentoJ.Last.SegmentoB.Items[x].DescOcorrencia;
              FPagFor.Registro0.Aviso.Last.Segmento        := 'J';
              FPagFor.Registro0.Aviso.Last.SegmentoFilho   := 'B';
              FPagFor.Registro0.Aviso.Last.SeuNumero       := FPagFor.Lote.Last.SegmentoJ.Last.ReferenciaSacado;
            end;
          end;

          for x := 0 to FPagFor.Lote.Last.SegmentoJ.Last.SegmentoC.Count - 1 do
          begin
            if POS(FPagFor.Lote.Last.SegmentoJ.Last.SegmentoC.Items[x].CodOcorrencia, PAGAMENTO_LIBERADO_AVISO) = 0 then
            begin
              FPagFor.Registro0.Aviso.New;
              FPagFor.Registro0.Aviso.Last.CodigoRetorno   := FPagFor.Lote.Last.SegmentoJ.Last.SegmentoC.Items[x].CodOcorrencia;
              FPagFor.Registro0.Aviso.Last.MensagemRetorno := FPagFor.Lote.Last.SegmentoJ.Last.SegmentoC.Items[x].DescOcorrencia;
              FPagFor.Registro0.Aviso.Last.Segmento        := 'J';
              FPagFor.Registro0.Aviso.Last.SegmentoFilho   := 'C';
              FPagFor.Registro0.Aviso.Last.SeuNumero       := FPagFor.Lote.Last.SegmentoJ.Last.ReferenciaSacado;
            end;
          end;
        end;
    end;
  end;
end;

procedure TPagForR.LerSegmentoJ52(mSegmentoJ52List: TSegmentoJ52List; I:Integer);
var
  mOk : Boolean;
begin
  if ((Copy(FArquivoTXT.Strings[i], 8, 1) + Copy(FArquivoTXT.Strings[i], 14, 1) + Copy(FArquivoTXT.Strings[i], 18, 2)) <> '3J52') then
    Exit;

  mSegmentoJ52List.New;
  mSegmentoJ52List.Last.TipoMovimento := StrToTpMovimento(mOk, Copy(FArquivoTXT.Strings[i], 15, 1));
  mSegmentoJ52List.Last.CodMovimento  := StrToInMovimento(mOk, Copy(FArquivoTXT.Strings[i], 16, 2));

  mSegmentoJ52List.Last.Pagador.Inscricao.Tipo   := StrToTpInscricao(mOk, Copy(FArquivoTXT.Strings[i], 20, 1));
  mSegmentoJ52List.Last.Pagador.Inscricao.Numero := Copy(FArquivoTXT.Strings[i], 21, 15);
  mSegmentoJ52List.Last.Pagador.Nome             := Copy(FArquivoTXT.Strings[i], 36, 40);

  mSegmentoJ52List.Last.Beneficiario.Inscricao.Tipo   := StrToTpInscricao(mOk, Copy(FArquivoTXT.Strings[i], 76, 1));
  mSegmentoJ52List.Last.Beneficiario.Inscricao.Numero := Copy(FArquivoTXT.Strings[i], 77, 15);
  mSegmentoJ52List.Last.Beneficiario.Nome             := Copy(FArquivoTXT.Strings[i], 92, 40);

  mSegmentoJ52List.Last.SacadorAvalista.Inscricao.Tipo   := StrToTpInscricao(mOk, Copy(FArquivoTXT.Strings[i], 132, 1));
  mSegmentoJ52List.Last.SacadorAvalista.Inscricao.Numero := Copy(FArquivoTXT.Strings[i], 133, 15);
  mSegmentoJ52List.Last.SacadorAvalista.Nome             := Copy(FArquivoTXT.Strings[i], 148, 40);
end;

procedure TPagForR.LerSegmentoJ99(mSegmentoJ99List: TSegmentoJ99List; I:Integer);
var
  mOk : Boolean;
begin
  if ((Copy(FArquivoTXT.Strings[i], 8, 1) + Copy(FArquivoTXT.Strings[i], 14, 1) + Copy(FArquivoTXT.Strings[i], 18, 2)) <> '3J99') then
    Exit;

  mSegmentoJ99List.New;
  mSegmentoJ99List.Last.CodMovimento := StrToInMovimento(mOk, Copy(FArquivoTXT.Strings[i], 16, 2));
  mSegmentoJ99List.Last.CodAutenticacao := StrToIntDef(Copy(FArquivoTXT.Strings[i], 20, 10 ), 0);
  mSegmentoJ99List.Last.NunDocumento := Copy(FArquivoTXT.Strings[i], 30, 25);

  mSegmentoJ99List.Last.DataHoraPagamento := StringToDateTime(Copy(FArquivoTXT.Strings[i], 55, 2) + '/' + Copy(FArquivoTXT.Strings[i], 57, 2) + '/' + Copy(FArquivoTXT.Strings[i], 59, 4) + ' ' +
                                                              Copy(FArquivoTXT.Strings[i], 63, 2) + ':' + Copy(FArquivoTXT.Strings[i], 65, 2) + ':' + Copy(FArquivoTXT.Strings[i], 67, 2));

  mSegmentoJ99List.Last.ProtocoloPagamento := Copy(FArquivoTXT.Strings[i], 69, 70);
end;

procedure TPagForR.LerSegmentoN1(I: Integer);
var
  mOk:Boolean;
  x:integer;
begin
  if ((Copy(FArquivoTXT.Strings[i], 8, 1) + Copy(FArquivoTXT.Strings[i], 14, 1)) <> '3N') then
    Exit;

  if (FPagFor.Geral.Banco = pagItau) and (Copy(FArquivoTXT.Strings[i], 18, 2) <> '01') then // Só processa se for GPS
    Exit;

  case FPagFor.Geral.Banco of
    pagItau:
      begin

        FPagFor.Lote.Last.SegmentoN1.New;
        FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.CodMovimento := TInstrucaoMovimento(StrToInt(Copy(FArquivoTXT.Strings[i], 15, 3)));

        FPagFor.Lote.Last.SegmentoN1.Last.CodigoPagamento          := StrToCodigoPagamentoGps(mOk, Copy(FArquivoTXT.Strings[i], 20, 4));
        FPagFor.Lote.Last.SegmentoN1.Last.MesAnoCompetencia        := StrToInt(Copy(FArquivoTXT.Strings[i], 24, 6));
        FPagFor.Lote.Last.SegmentoN1.Last.idContribuinte           := Copy(FArquivoTXT.Strings[i], 30, 14);
        FPagFor.Lote.Last.SegmentoN1.Last.ValorTributo             := StrToInt(Copy(FArquivoTXT.Strings[i], 44, 14)) / 100;
        FPagFor.Lote.Last.SegmentoN1.Last.ValorOutrasEntidades     := StrToInt(Copy(FArquivoTXT.Strings[i], 58, 14)) / 100;
        FPagFor.Lote.Last.SegmentoN1.Last.AtualizacaoMonetaria     := StrToInt(Copy(FArquivoTXT.Strings[i], 72, 14)) / 100;
        FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.ValorPagamento := StrToInt(Copy(FArquivoTXT.Strings[i], 86, 14)) / 100;
        FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.DataPagamento  := StringToDateTime(Copy(FArquivoTXT.Strings[i], 100, 2)+'/'+Copy(FArquivoTXT.Strings[i], 102, 2)+'/'+Copy(FArquivoTXT.Strings[i], 104, 4));
        FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.SeuNumero      := Copy(FArquivoTXT.Strings[i], 196, 20);
        FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.NossoNumero    := Copy(FArquivoTXT.Strings[i], 216, 20);
        FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.CodOcorrencia  := Trim(Copy(FArquivoTXT.Strings[i], 231, 10));
        FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.DescOcorrencia := DescricaoRetornoItau(FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.CodOcorrencia);

        if POS(FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.CodOcorrencia, PAGAMENTO_LIBERADO_AVISO) = 0 then
        begin
          FPagFor.Registro0.Aviso.New;
          FPagFor.Registro0.Aviso.Last.CodigoRetorno   := FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.CodOcorrencia;
          FPagFor.Registro0.Aviso.Last.MensagemRetorno := FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.DescOcorrencia;
          FPagFor.Registro0.Aviso.Last.Segmento        := 'N';
          FPagFor.Registro0.Aviso.Last.SegmentoFilho   := '';
          FPagFor.Registro0.Aviso.Last.SeuNumero       := FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.SeuNumero;
        end;
      end
  else
    begin
      //Tributo N1 - GPS
      if StrToInt(Copy(FArquivoTXT.Strings[i], 133, 2)) <> 17 then
        Exit;

      FPagFor.Lote.Last.SegmentoN1.New;
      FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.CodMovimento := TInstrucaoMovimento(StrToInt(Copy(FArquivoTXT.Strings[i], 15, 3)));

      FPagFor.Lote.Last.SegmentoN1.Last.Receita                  := StrToInt(Copy(FArquivoTXT.Strings[i], 111, 6));
      FPagFor.Lote.Last.SegmentoN1.Last.TipoContribuinte         := StrToTpInscricao(mOk, Copy(FArquivoTXT.Strings[i], 117, 2));
      FPagFor.Lote.Last.SegmentoN1.Last.idContribuinte           := Copy(FArquivoTXT.Strings[i], 119, 14);
      FPagFor.Lote.Last.SegmentoN1.Last.MesAnoCompetencia        := StrToInt(Copy(FArquivoTXT.Strings[i], 135, 6));
      FPagFor.Lote.Last.SegmentoN1.Last.ValorTributo             := StrToInt(Copy(FArquivoTXT.Strings[i], 141, 14)) / 100;
      FPagFor.Lote.Last.SegmentoN1.Last.ValorOutrasEntidades     := StrToInt(Copy(FArquivoTXT.Strings[i], 156, 14)) / 100;
      FPagFor.Lote.Last.SegmentoN1.Last.AtualizacaoMonetaria     := StrToInt(Copy(FArquivoTXT.Strings[i], 171, 14)) / 100;
      FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.ValorPagamento := StrToInt(Copy(FArquivoTXT.Strings[i], 96, 14)) / 100;
      FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.DataPagamento  := StringToDateTime(Copy(FArquivoTXT.Strings[i], 88, 2)+'/'+Copy(FArquivoTXT.Strings[i], 90, 2)+'/'+Copy(FArquivoTXT.Strings[i], 92, 4));
      FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.SeuNumero      := Copy(FArquivoTXT.Strings[i], 18, 20);
      FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.NossoNumero    := Copy(FArquivoTXT.Strings[i], 38, 20);
      FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.CodOcorrencia  := Trim(Copy(FArquivoTXT.Strings[i], 231, 10));
      FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.DescOcorrencia := DescricaoRetornoItau(FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.CodOcorrencia);

      if POS(FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.CodOcorrencia, PAGAMENTO_LIBERADO_AVISO) = 0 then
      begin
        FPagFor.Registro0.Aviso.New;
        FPagFor.Registro0.Aviso.Last.CodigoRetorno   := FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.CodOcorrencia;
        FPagFor.Registro0.Aviso.Last.MensagemRetorno := FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.DescOcorrencia;
        FPagFor.Registro0.Aviso.Last.Segmento        := 'N';
        FPagFor.Registro0.Aviso.Last.SegmentoFilho   := '';
        FPagFor.Registro0.Aviso.Last.SeuNumero       := FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.SeuNumero;
      end;
    end;
  end;

  {Adicionais segmento N}
  LerSegmentoB(FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.SegmentoB, i);
  LerSegmentoW(FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.SegmentoW, i);
  LerSegmentoZ(FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.SegmentoZ, i);

  case FPagFor.Geral.Banco of
    pagItau:
      begin
        for x := 0 to FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.SegmentoB.Count - 1 do
        begin
          if POS(FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.SegmentoB.Items[x].CodOcorrencia, PAGAMENTO_LIBERADO_AVISO) = 0 then
          begin
            FPagFor.Registro0.Aviso.New;
            FPagFor.Registro0.Aviso.Last.CodigoRetorno   := FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.CodOcorrencia;
            FPagFor.Registro0.Aviso.Last.MensagemRetorno := FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.DescOcorrencia;
            FPagFor.Registro0.Aviso.Last.Segmento        := 'N';
            FPagFor.Registro0.Aviso.Last.SegmentoFilho   := 'B';
            FPagFor.Registro0.Aviso.Last.SeuNumero       := FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.SeuNumero;
          end;
        end;
      end;
  end;
end;

procedure TPagForR.LerSegmentoN2(I: Integer);
var
  mOk:boolean;
  x:integer;
begin
  if ((Copy(FArquivoTXT.Strings[i], 8, 1) + Copy(FArquivoTXT.Strings[i], 14, 1)) <> '3N') then
    Exit;

  if (FPagFor.Geral.Banco = pagItau) and (Copy(FArquivoTXT.Strings[i], 18, 2) <> '02') then // Só processa se for DARF
    Exit;

  case FPagFor.Geral.Banco of
    pagItau:
      begin

        FPagFor.Lote.Last.SegmentoN2.New;
        FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.CodMovimento := StrToInMovimento(mOk, Copy(FArquivoTXT.Strings[i], 0, 0));

        FPagFor.Lote.Last.SegmentoN2.Last.Receita                    := StrToInt(Copy(FArquivoTXT.Strings[i], 20, 4));
        FPagFor.Lote.Last.SegmentoN2.Last.TipoContribuinte           := StrToTpInscricao(mOk, Copy(FArquivoTXT.Strings[i], 24, 1));
        FPagFor.Lote.Last.SegmentoN2.Last.idContribuinte             := Copy(FArquivoTXT.Strings[i], 25, 14);
        FPagFor.Lote.Last.SegmentoN2.Last.Periodo                    := StringToDateTime(Copy(FArquivoTXT.Strings[i], 39, 2)+'/'+Copy(FArquivoTXT.Strings[i], 41, 2)+'/'+Copy(FArquivoTXT.Strings[i], 43, 4));
        FPagFor.Lote.Last.SegmentoN2.Last.Referencia                 := Copy(FArquivoTXT.Strings[i], 47, 17);
        FPagFor.Lote.Last.SegmentoN2.Last.ValorPrincipal             := (StrToInt(Copy(FArquivoTXT.Strings[i], 64, 14)) / 100);
        FPagFor.Lote.Last.SegmentoN2.Last.Multa                      := (StrToInt(Copy(FArquivoTXT.Strings[i], 78, 14)) / 100);
        FPagFor.Lote.Last.SegmentoN2.Last.Juros                      := (StrToInt(Copy(FArquivoTXT.Strings[i], 92, 14)) / 100);
        FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.ValorPagamento   := (StrToInt(Copy(FArquivoTXT.Strings[i], 106, 14)) / 100);
        FPagFor.Lote.Last.SegmentoN2.Last.DataVencimento             := StringToDateTime(Copy(FArquivoTXT.Strings[i], 120, 2)+'/'+Copy(FArquivoTXT.Strings[i], 122, 2)+'/'+Copy(FArquivoTXT.Strings[i], 124, 4));
        FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.DataPagamento    := StringToDateTime(Copy(FArquivoTXT.Strings[i], 128, 2)+'/'+Copy(FArquivoTXT.Strings[i], 130, 2)+'/'+Copy(FArquivoTXT.Strings[i], 132, 4));
        FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.NomeContribuinte := Copy(FArquivoTXT.Strings[i], 166, 30);
        FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.SeuNumero        := Copy(FArquivoTXT.Strings[i], 196, 20);

        FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.CodOcorrencia  := Trim(Copy(FArquivoTXT.Strings[i], 231, 10));
        FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.DescOcorrencia := DescricaoRetornoItau(FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.CodOcorrencia);

        if POS(FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.CodOcorrencia, PAGAMENTO_LIBERADO_AVISO) = 0 then
        begin
          FPagFor.Registro0.Aviso.New;
          FPagFor.Registro0.Aviso.Last.CodigoRetorno   := FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.CodOcorrencia;
          FPagFor.Registro0.Aviso.Last.MensagemRetorno := FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.DescOcorrencia;
          FPagFor.Registro0.Aviso.Last.Segmento        := 'N';
          FPagFor.Registro0.Aviso.Last.SegmentoFilho   := '';
          FPagFor.Registro0.Aviso.Last.SeuNumero       := FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.SeuNumero;
        end;
      end
  else
    begin
      //Tributo N2 - DARF NORMAL
      if StrToInt(Copy(FArquivoTXT.Strings[i], 133, 2)) <> 16 then
        Exit;

      FPagFor.Lote.Last.SegmentoN2.New;
      FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.CodMovimento := StrToInMovimento(mOk, Copy(FArquivoTXT.Strings[i], 0, 0));

      FPagFor.Lote.Last.SegmentoN2.Last.Receita                    := StrToInt(Copy(FArquivoTXT.Strings[i], 111, 6));
      FPagFor.Lote.Last.SegmentoN2.Last.TipoContribuinte           := StrToTpInscricao(mOk, Copy(FArquivoTXT.Strings[i], 117, 2));
      FPagFor.Lote.Last.SegmentoN2.Last.idContribuinte             := Copy(FArquivoTXT.Strings[i], 119, 14);
      FPagFor.Lote.Last.SegmentoN2.Last.Periodo                    := StringToDateTime(Copy(FArquivoTXT.Strings[i], 135, 2)+'/'+Copy(FArquivoTXT.Strings[i], 137, 2)+'/'+Copy(FArquivoTXT.Strings[i], 138, 4));
      FPagFor.Lote.Last.SegmentoN2.Last.Referencia                 := Copy(FArquivoTXT.Strings[i], 143, 17);
      FPagFor.Lote.Last.SegmentoN2.Last.ValorPrincipal             := (StrToInt(Copy(FArquivoTXT.Strings[i], 160, 15)) / 100);
      FPagFor.Lote.Last.SegmentoN2.Last.Multa                      := (StrToInt(Copy(FArquivoTXT.Strings[i], 175, 15)) / 100);
      FPagFor.Lote.Last.SegmentoN2.Last.Juros                      := (StrToInt(Copy(FArquivoTXT.Strings[i], 190, 15)) / 100);
      FPagFor.Lote.Last.SegmentoN2.Last.DataVencimento             := StringToDateTime(Copy(FArquivoTXT.Strings[i], 205, 2)+'/'+Copy(FArquivoTXT.Strings[i], 207, 2)+'/'+Copy(FArquivoTXT.Strings[i], 209, 4));

      FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.ValorPagamento := StrToInt(Copy(FArquivoTXT.Strings[i], 96, 14)) / 100;
      FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.DataPagamento  := StringToDateTime(Copy(FArquivoTXT.Strings[i], 88, 2)+'/'+Copy(FArquivoTXT.Strings[i], 90, 2)+'/'+Copy(FArquivoTXT.Strings[i], 92, 4));
      FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.SeuNumero      := Copy(FArquivoTXT.Strings[i], 18, 20);
      FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.NossoNumero    := Copy(FArquivoTXT.Strings[i], 38, 20);
      FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.CodOcorrencia  := Trim(Copy(FArquivoTXT.Strings[i], 231, 10));
      FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.DescOcorrencia := DescricaoRetornoItau(FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.CodOcorrencia);

      if POS(FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.CodOcorrencia, PAGAMENTO_LIBERADO_AVISO) = 0 then
      begin
        FPagFor.Registro0.Aviso.New;
        FPagFor.Registro0.Aviso.Last.CodigoRetorno   := FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.CodOcorrencia;
        FPagFor.Registro0.Aviso.Last.MensagemRetorno := FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.DescOcorrencia;
        FPagFor.Registro0.Aviso.Last.Segmento        := 'N';
        FPagFor.Registro0.Aviso.Last.SegmentoFilho   := '';
        FPagFor.Registro0.Aviso.Last.SeuNumero       := FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.SeuNumero;
      end;
    end;
  end;

  {Adicionais segmento N}
  LerSegmentoB(FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.SegmentoB, i);
  LerSegmentoW(FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.SegmentoW, i);
  LerSegmentoZ(FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.SegmentoZ, i);

  case FPagFor.Geral.Banco of
    pagItau:
      begin
        for x := 0 to FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.SegmentoB.Count - 1 do
        begin
          if POS(FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.SegmentoB.Items[x].CodOcorrencia, PAGAMENTO_LIBERADO_AVISO) = 0 then
          begin
            FPagFor.Registro0.Aviso.New;
            FPagFor.Registro0.Aviso.Last.CodigoRetorno   := FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.SegmentoB.Items[x].CodOcorrencia;
            FPagFor.Registro0.Aviso.Last.MensagemRetorno := FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.SegmentoB.Items[x].DescOcorrencia;
            FPagFor.Registro0.Aviso.Last.Segmento        := 'N';
            FPagFor.Registro0.Aviso.Last.SegmentoFilho   := 'B';
            FPagFor.Registro0.Aviso.Last.SeuNumero       := FPagFor.Lote.Last.SegmentoN1.Last.SegmentoN.SeuNumero;
          end;
        end;
      end;
  end;
end;

procedure TPagForR.LerSegmentoN3(I: Integer);
var
  mOk: boolean;
  x:integer;
begin
  if ((Copy(FArquivoTXT.Strings[i], 8, 1) + Copy(FArquivoTXT.Strings[i], 14, 1)) <> '3N') then
    Exit;

  if (FPagFor.Geral.Banco = pagItau) and (Copy(FArquivoTXT.Strings[i], 18, 2) <> '03') then // Só processa se for DARF Simples
    Exit;

  case FPagFor.Geral.Banco of
    pagItau:
      begin

        FPagFor.Lote.Last.SegmentoN3.New;
        FPagFor.Lote.Last.SegmentoN3.Last.SegmentoN.CodMovimento := StrToInMovimento(mOk, Copy(FArquivoTXT.Strings[i], 0, 0));

        FPagFor.Lote.Last.SegmentoN3.Last.Receita                    := StrToInt(Copy(FArquivoTXT.Strings[i], 0, 0));
        FPagFor.Lote.Last.SegmentoN3.Last.TipoContribuinte           := StrToTpInscricao(mOk, Copy(FArquivoTXT.Strings[i], 0, 0));
        FPagFor.Lote.Last.SegmentoN3.Last.idContribuinte             := Copy(FArquivoTXT.Strings[i], 0, 0);
        FPagFor.Lote.Last.SegmentoN3.Last.Periodo                    := StringToDateTime(Copy(FArquivoTXT.Strings[i], 0, 2)+'/'+Copy(FArquivoTXT.Strings[i], 0, 2)+'/'+Copy(FArquivoTXT.Strings[i], 0, 4));
        FPagFor.Lote.Last.SegmentoN3.Last.ReceitaBruta               := StrToInt(Copy(FArquivoTXT.Strings[i], 0, 0)) / 100;
        FPagFor.Lote.Last.SegmentoN3.Last.Percentual                 := StrToInt(Copy(FArquivoTXT.Strings[i], 0, 0)) / 100;
        FPagFor.Lote.Last.SegmentoN3.Last.ValorPrincipal             := StrToInt(Copy(FArquivoTXT.Strings[i], 0, 0)) / 100;
        FPagFor.Lote.Last.SegmentoN3.Last.Multa                      := StrToInt(Copy(FArquivoTXT.Strings[i], 0, 0)) / 100;
        FPagFor.Lote.Last.SegmentoN3.Last.Juros                      := StrToInt(Copy(FArquivoTXT.Strings[i], 0, 0)) / 100;
        FPagFor.Lote.Last.SegmentoN3.Last.SegmentoN.ValorPagamento   := StrToInt(Copy(FArquivoTXT.Strings[i], 0, 0)) / 100;
        FPagFor.Lote.Last.SegmentoN3.Last.DataVencimento             := StringToDateTime(Copy(FArquivoTXT.Strings[i], 0, 2)+'/'+Copy(FArquivoTXT.Strings[i], 0, 2)+'/'+Copy(FArquivoTXT.Strings[i], 0, 4));
        FPagFor.Lote.Last.SegmentoN3.Last.SegmentoN.DataPagamento    := StringToDateTime(Copy(FArquivoTXT.Strings[i], 0, 2)+'/'+Copy(FArquivoTXT.Strings[i], 0, 2)+'/'+Copy(FArquivoTXT.Strings[i], 0, 4));
        FPagFor.Lote.Last.SegmentoN3.Last.SegmentoN.NomeContribuinte := Copy(FArquivoTXT.Strings[i], 0, 0);
        FPagFor.Lote.Last.SegmentoN3.Last.SegmentoN.SeuNumero        := Copy(FArquivoTXT.Strings[i], 0, 0);

        FPagFor.Lote.Last.SegmentoN3.Last.SegmentoN.CodOcorrencia  := Trim(Copy(FArquivoTXT.Strings[i], 231, 10));
        FPagFor.Lote.Last.SegmentoN3.Last.SegmentoN.DescOcorrencia := DescricaoRetornoItau(FPagFor.Lote.Last.SegmentoN3.Last.SegmentoN.CodOcorrencia);

        if POS(FPagFor.Lote.Last.SegmentoN3.Last.SegmentoN.CodOcorrencia, PAGAMENTO_LIBERADO_AVISO) = 0 then
        begin
          FPagFor.Registro0.Aviso.New;
          FPagFor.Registro0.Aviso.Last.CodigoRetorno   := FPagFor.Lote.Last.SegmentoN3.Last.SegmentoN.CodOcorrencia;
          FPagFor.Registro0.Aviso.Last.MensagemRetorno := FPagFor.Lote.Last.SegmentoN3.Last.SegmentoN.DescOcorrencia;
          FPagFor.Registro0.Aviso.Last.Segmento        := 'N';
          FPagFor.Registro0.Aviso.Last.SegmentoFilho   := '';
          FPagFor.Registro0.Aviso.Last.SeuNumero       := FPagFor.Lote.Last.SegmentoN3.Last.SegmentoN.SeuNumero;
        end;
      end
  else
    begin
      //Tributo N3 - DARF SIMPLES
      if StrToInt(Copy(FArquivoTXT.Strings[i], 133, 2)) <> 18 then
         Exit;

      FPagFor.Lote.Last.SegmentoN3.New;
      FPagFor.Lote.Last.SegmentoN3.Last.SegmentoN.CodMovimento := StrToInMovimento(mOk, Copy(FArquivoTXT.Strings[i], 0, 0));

      FPagFor.Lote.Last.SegmentoN3.Last.Receita                    := StrToInt(Copy(FArquivoTXT.Strings[i], 111, 6));
      FPagFor.Lote.Last.SegmentoN3.Last.TipoContribuinte           := StrToTpInscricao(mOk, Copy(FArquivoTXT.Strings[i], 117, 2));
      FPagFor.Lote.Last.SegmentoN3.Last.idContribuinte             := Copy(FArquivoTXT.Strings[i], 119, 14);
      FPagFor.Lote.Last.SegmentoN3.Last.Periodo                    := StringToDateTime(Copy(FArquivoTXT.Strings[i], 135, 2)+'/'+Copy(FArquivoTXT.Strings[i], 137, 2)+'/'+Copy(FArquivoTXT.Strings[i], 138, 4));
      FPagFor.Lote.Last.SegmentoN3.Last.ReceitaBruta               := (StrToInt(Copy(FArquivoTXT.Strings[i], 143, 15)) / 100);
      FPagFor.Lote.Last.SegmentoN3.Last.Percentual                 := (StrToInt(Copy(FArquivoTXT.Strings[i], 158, 7)) / 100);
      FPagFor.Lote.Last.SegmentoN3.Last.ValorPrincipal             := (StrToInt(Copy(FArquivoTXT.Strings[i], 165, 15)) / 100);
      FPagFor.Lote.Last.SegmentoN3.Last.Multa                      := (StrToInt(Copy(FArquivoTXT.Strings[i], 180, 15)) / 100);
      FPagFor.Lote.Last.SegmentoN3.Last.Juros                      := (StrToInt(Copy(FArquivoTXT.Strings[i], 195, 15)) / 100);
      //FPagFor.Lote.Last.SegmentoN3.Last.DataVencimento             := StringToDateTime(Copy(FArquivoTXT.Strings[i], 0, 2)+'/'+Copy(FArquivoTXT.Strings[i], 0, 2)+'/'+Copy(FArquivoTXT.Strings[i], 0, 4));

      FPagFor.Lote.Last.SegmentoN3.Last.SegmentoN.ValorPagamento := StrToInt(Copy(FArquivoTXT.Strings[i], 96, 14)) / 100;
      FPagFor.Lote.Last.SegmentoN3.Last.SegmentoN.DataPagamento  := StringToDateTime(Copy(FArquivoTXT.Strings[i], 88, 2)+'/'+Copy(FArquivoTXT.Strings[i], 90, 2)+'/'+Copy(FArquivoTXT.Strings[i], 92, 4));
      FPagFor.Lote.Last.SegmentoN3.Last.SegmentoN.SeuNumero      := Copy(FArquivoTXT.Strings[i], 18, 20);
      FPagFor.Lote.Last.SegmentoN3.Last.SegmentoN.NossoNumero    := Copy(FArquivoTXT.Strings[i], 38, 20);
      FPagFor.Lote.Last.SegmentoN3.Last.SegmentoN.CodOcorrencia  := Trim(Copy(FArquivoTXT.Strings[i], 231, 10));
      FPagFor.Lote.Last.SegmentoN3.Last.SegmentoN.DescOcorrencia := DescricaoRetornoItau(FPagFor.Lote.Last.SegmentoN3.Last.SegmentoN.CodOcorrencia);


      if POS(FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.CodOcorrencia, PAGAMENTO_LIBERADO_AVISO) = 0 then
      begin
        FPagFor.Registro0.Aviso.New;
        FPagFor.Registro0.Aviso.Last.CodigoRetorno   := FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.CodOcorrencia;
        FPagFor.Registro0.Aviso.Last.MensagemRetorno := FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.DescOcorrencia;
        FPagFor.Registro0.Aviso.Last.Segmento        := 'N';
        FPagFor.Registro0.Aviso.Last.SegmentoFilho   := '';
        FPagFor.Registro0.Aviso.Last.SeuNumero       := FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.SeuNumero;
      end;
    end;
  end;

  {Adicionais segmento N}
  LerSegmentoB(FPagFor.Lote.Last.SegmentoN3.Last.SegmentoN.SegmentoB, i);
  LerSegmentoW(FPagFor.Lote.Last.SegmentoN3.Last.SegmentoN.SegmentoW, i);
  LerSegmentoZ(FPagFor.Lote.Last.SegmentoN3.Last.SegmentoN.SegmentoZ, i);

  case FPagFor.Geral.Banco of
    pagItau:
      begin
        for x := 0 to FPagFor.Lote.Last.SegmentoN3.Last.SegmentoN.SegmentoB.Count - 1 do
        begin
          if POS(FPagFor.Lote.Last.SegmentoN3.Last.SegmentoN.SegmentoB.Items[x].CodOcorrencia, PAGAMENTO_LIBERADO_AVISO) = 0 then
          begin
            FPagFor.Registro0.Aviso.New;
            FPagFor.Registro0.Aviso.Last.CodigoRetorno   := FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.SegmentoB.Items[x].CodOcorrencia;
            FPagFor.Registro0.Aviso.Last.MensagemRetorno := FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.SegmentoB.Items[x].DescOcorrencia;
            FPagFor.Registro0.Aviso.Last.Segmento        := 'N';
            FPagFor.Registro0.Aviso.Last.SegmentoFilho   := 'B';
            FPagFor.Registro0.Aviso.Last.SeuNumero       := FPagFor.Lote.Last.SegmentoN2.Last.SegmentoN.SeuNumero;
          end;
        end;
      end;
  end;
end;

procedure TPagForR.LerSegmentoN4(I: Integer);
var
 mOk:boolean;
 x:integer;
begin
  if ((Copy(FArquivoTXT.Strings[i], 8, 1) + Copy(FArquivoTXT.Strings[i], 14, 1)) <> '3N') then
    Exit;

  if (FPagFor.Geral.Banco = pagItau) and (Copy(FArquivoTXT.Strings[i], 18, 2) <> '05') then // Só processa se for GARE SP ICMS
    Exit;

  if (FPagFor.Geral.Banco <> pagItau) then // Teste para sair se for outro Banco
    Exit;

  case FPagFor.Geral.Banco of
    pagItau:
      begin

        FPagFor.Lote.Last.SegmentoN4.New;
        FPagFor.Lote.Last.SegmentoN4.Last.SegmentoN.CodMovimento := StrToInMovimento(mOk, Copy(FArquivoTXT.Strings[i], 0, 0));

        FPagFor.Lote.Last.SegmentoN4.Last.Receita                    := StrToInt(Copy(FArquivoTXT.Strings[i], 20, 4));
        FPagFor.Lote.Last.SegmentoN4.Last.TipoContribuinte           := StrToTpInscricao(mOk, Copy(FArquivoTXT.Strings[i], 24, 1));
        FPagFor.Lote.Last.SegmentoN4.Last.idContribuinte             := Copy(FArquivoTXT.Strings[i], 25, 14);
        FPagFor.Lote.Last.SegmentoN4.Last.InscEst                    := Copy(FArquivoTXT.Strings[i], 39, 12);
        FPagFor.Lote.Last.SegmentoN4.Last.NumEtiqueta                := Copy(FArquivoTXT.Strings[i], 51, 13);
        FPagFor.Lote.Last.SegmentoN4.Last.Referencia                 := StrToInt(Copy(FArquivoTXT.Strings[i], 64, 6));
        FPagFor.Lote.Last.SegmentoN4.Last.NumParcela                 := Copy(FArquivoTXT.Strings[i], 70, 13);
        FPagFor.Lote.Last.SegmentoN4.Last.ValorReceita               := StrToInt(Copy(FArquivoTXT.Strings[i], 83, 14)) / 100;
        FPagFor.Lote.Last.SegmentoN4.Last.Juros                      := StrToInt(Copy(FArquivoTXT.Strings[i], 97, 14)) / 100;
        FPagFor.Lote.Last.SegmentoN4.Last.Multa                      := StrToInt(Copy(FArquivoTXT.Strings[i], 111, 14)) / 100;
        FPagFor.Lote.Last.SegmentoN4.Last.SegmentoN.ValorPagamento   := StrToInt(Copy(FArquivoTXT.Strings[i], 125, 14)) / 100;
        FPagFor.Lote.Last.SegmentoN4.Last.DataVencimento             := StringToDateTime(Copy(FArquivoTXT.Strings[i], 139, 2)+'/'+Copy(FArquivoTXT.Strings[i], 141, 2)+'/'+Copy(FArquivoTXT.Strings[i], 143, 4));
        FPagFor.Lote.Last.SegmentoN4.Last.SegmentoN.DataPagamento    := StringToDateTime(Copy(FArquivoTXT.Strings[i], 147, 2)+'/'+Copy(FArquivoTXT.Strings[i], 149, 2)+'/'+Copy(FArquivoTXT.Strings[i], 151, 4));
        FPagFor.Lote.Last.SegmentoN4.Last.SegmentoN.NomeContribuinte := Copy(FArquivoTXT.Strings[i], 166, 30);
        FPagFor.Lote.Last.SegmentoN4.Last.SegmentoN.SeuNumero        := Copy(FArquivoTXT.Strings[i], 196, 20);
        FPagFor.Lote.Last.SegmentoN4.Last.SegmentoN.NossoNumero      := Copy(FArquivoTXT.Strings[i], 216, 15);

        FPagFor.Lote.Last.SegmentoN4.Last.SegmentoN.CodOcorrencia  := Trim(Copy(FArquivoTXT.Strings[i], 231, 10));
        FPagFor.Lote.Last.SegmentoN4.Last.SegmentoN.DescOcorrencia := DescricaoRetornoItau(FPagFor.Lote.Last.SegmentoN4.Last.SegmentoN.CodOcorrencia);

        if POS(FPagFor.Lote.Last.SegmentoN4.Last.SegmentoN.CodOcorrencia, PAGAMENTO_LIBERADO_AVISO) = 0 then
        begin
          FPagFor.Registro0.Aviso.New;
          FPagFor.Registro0.Aviso.Last.CodigoRetorno   := FPagFor.Lote.Last.SegmentoN4.Last.SegmentoN.CodOcorrencia;
          FPagFor.Registro0.Aviso.Last.MensagemRetorno := FPagFor.Lote.Last.SegmentoN4.Last.SegmentoN.DescOcorrencia;
          FPagFor.Registro0.Aviso.Last.Segmento        := 'N';
          FPagFor.Registro0.Aviso.Last.SegmentoFilho   := '';
          FPagFor.Registro0.Aviso.Last.SeuNumero       := FPagFor.Lote.Last.SegmentoN4.Last.SegmentoN.SeuNumero;
        end;
      end
  end;

  {Adicionais segmento N}
  LerSegmentoB(FPagFor.Lote.Last.SegmentoN4.Last.SegmentoN.SegmentoB, i);
  LerSegmentoW(FPagFor.Lote.Last.SegmentoN4.Last.SegmentoN.SegmentoW, i);
  LerSegmentoZ(FPagFor.Lote.Last.SegmentoN4.Last.SegmentoN.SegmentoZ, i);

  case FPagFor.Geral.Banco of
    pagItau:
      begin
        for x := 0 to FPagFor.Lote.Last.SegmentoN4.Last.SegmentoN.SegmentoB.Count - 1 do
        begin
          if POS(FPagFor.Lote.Last.SegmentoN4.Last.SegmentoN.SegmentoB.Items[x].CodOcorrencia, PAGAMENTO_LIBERADO_AVISO) = 0 then
          begin
            FPagFor.Registro0.Aviso.New;
            FPagFor.Registro0.Aviso.Last.CodigoRetorno   := FPagFor.Lote.Last.SegmentoN4.Last.SegmentoN.SegmentoB.Items[x].CodOcorrencia;
            FPagFor.Registro0.Aviso.Last.MensagemRetorno := FPagFor.Lote.Last.SegmentoN4.Last.SegmentoN.SegmentoB.Items[x].DescOcorrencia;
            FPagFor.Registro0.Aviso.Last.Segmento        := 'N';
            FPagFor.Registro0.Aviso.Last.SegmentoFilho   := 'B';
            FPagFor.Registro0.Aviso.Last.SeuNumero       := FPagFor.Lote.Last.SegmentoN4.Last.SegmentoN.SeuNumero;
          end;
        end;
      end;
  end;
end;

procedure TPagForR.LerSegmentoN567(I: Integer);
var
  mOk:boolean;
  x:integer;
begin
  if ((Copy(FArquivoTXT.Strings[i], 8, 1) + Copy(FArquivoTXT.Strings[i], 14, 1)) <> '3N') then
    Exit;

  if (FPagFor.Geral.Banco = pagItau) and (Pos(Copy(FArquivoTXT.Strings[i], 18, 2), '07 08') = 0) then // Só processa se for IPVA/DPVAT
    Exit;

  if (FPagFor.Geral.Banco <> pagItau) then // Teste para sair se for outro Banco
    Exit;


  case FPagFor.Geral.Banco of
    pagItau:
      begin

        FPagFor.Lote.Last.SegmentoN567.New;
        FPagFor.Lote.Last.SegmentoN567.Last.SegmentoN.CodMovimento := StrToInMovimento(mOk, Copy(FArquivoTXT.Strings[i], 0, 0));

        FPagFor.Lote.Last.SegmentoN567.Last.TipoContribuinte           := StrToTpInscricao(mOk, Copy(FArquivoTXT.Strings[i], 0, 0));
        FPagFor.Lote.Last.SegmentoN567.Last.idContribuinte             := Copy(FArquivoTXT.Strings[i], 0, 0);
        FPagFor.Lote.Last.SegmentoN567.Last.Exercicio                  := StrToInt(Copy(FArquivoTXT.Strings[i], 0, 0));
        FPagFor.Lote.Last.SegmentoN567.Last.Renavam                    := Copy(FArquivoTXT.Strings[i], 0, 0);
        FPagFor.Lote.Last.SegmentoN567.Last.Estado                     := Copy(FArquivoTXT.Strings[i], 0, 0);
        FPagFor.Lote.Last.SegmentoN567.Last.Municipio                  := StrToInt(Copy(FArquivoTXT.Strings[i], 0, 0));
        FPagFor.Lote.Last.SegmentoN567.Last.Placa                      := Copy(FArquivoTXT.Strings[i], 0, 0);
        FPagFor.Lote.Last.SegmentoN567.Last.OpcaoPagamento             := Copy(FArquivoTXT.Strings[i], 0, 0);
        FPagFor.Lote.Last.SegmentoN567.Last.ValorTributo               := StrToInt(Copy(FArquivoTXT.Strings[i], 0, 0)) / 100;
        FPagFor.Lote.Last.SegmentoN567.Last.Desconto                   := StrToInt(Copy(FArquivoTXT.Strings[i], 0, 0)) / 100;
        FPagFor.Lote.Last.SegmentoN567.Last.SegmentoN.ValorPagamento   := StrToInt(Copy(FArquivoTXT.Strings[i], 0, 0)) / 100;
        FPagFor.Lote.Last.SegmentoN567.Last.DataVencimento             := StringToDateTime(Copy(FArquivoTXT.Strings[i], 0, 2)+'/'+Copy(FArquivoTXT.Strings[i], 0, 2)+'/'+Copy(FArquivoTXT.Strings[i], 0, 4));
        FPagFor.Lote.Last.SegmentoN567.Last.SegmentoN.DataPagamento    := StringToDateTime(Copy(FArquivoTXT.Strings[i], 0, 2)+'/'+Copy(FArquivoTXT.Strings[i], 0, 2)+'/'+Copy(FArquivoTXT.Strings[i], 0, 4));
//        FPagFor.Lote.Last.SegmentoN567.Last.Renavam  12
        FPagFor.Lote.Last.SegmentoN567.Last.SegmentoN.NomeContribuinte := Copy(FArquivoTXT.Strings[i], 0, 0);
        FPagFor.Lote.Last.SegmentoN567.Last.SegmentoN.SeuNumero        := Copy(FArquivoTXT.Strings[i], 0, 0);

        FPagFor.Lote.Last.SegmentoN567.Last.SegmentoN.CodOcorrencia  := Trim(Copy(FArquivoTXT.Strings[i], 231, 10));
        FPagFor.Lote.Last.SegmentoN567.Last.SegmentoN.DescOcorrencia := DescricaoRetornoItau(FPagFor.Lote.Last.SegmentoN567.Last.SegmentoN.CodOcorrencia);

        if POS(FPagFor.Lote.Last.SegmentoN567.Last.SegmentoN.CodOcorrencia, PAGAMENTO_LIBERADO_AVISO) = 0 then
        begin
          FPagFor.Registro0.Aviso.New;
          FPagFor.Registro0.Aviso.Last.CodigoRetorno   := FPagFor.Lote.Last.SegmentoN567.Last.SegmentoN.CodOcorrencia;
          FPagFor.Registro0.Aviso.Last.MensagemRetorno := FPagFor.Lote.Last.SegmentoN567.Last.SegmentoN.DescOcorrencia;
          FPagFor.Registro0.Aviso.Last.Segmento        := 'N';
          FPagFor.Registro0.Aviso.Last.SegmentoFilho   := '';
          FPagFor.Registro0.Aviso.Last.SeuNumero       := FPagFor.Lote.Last.SegmentoN567.Last.SegmentoN.SeuNumero;
        end;
      end
  end;

  {Adicionais segmento N}
  LerSegmentoB(FPagFor.Lote.Last.SegmentoN567.Last.SegmentoN.SegmentoB, i);
  LerSegmentoW(FPagFor.Lote.Last.SegmentoN567.Last.SegmentoN.SegmentoW, i);
  LerSegmentoZ(FPagFor.Lote.Last.SegmentoN567.Last.SegmentoN.SegmentoZ, i);

  case FPagFor.Geral.Banco of
    pagItau:
      begin
        for x := 0 to FPagFor.Lote.Last.SegmentoN567.Last.SegmentoN.SegmentoB.Count - 1 do
        begin
          if POS(FPagFor.Lote.Last.SegmentoN567.Last.SegmentoN.SegmentoB.Items[x].CodOcorrencia, PAGAMENTO_LIBERADO_AVISO) = 0 then
          begin
            FPagFor.Registro0.Aviso.New;
            FPagFor.Registro0.Aviso.Last.CodigoRetorno   := FPagFor.Lote.Last.SegmentoN567.Last.SegmentoN.SegmentoB.Items[x].CodOcorrencia;
            FPagFor.Registro0.Aviso.Last.MensagemRetorno := FPagFor.Lote.Last.SegmentoN567.Last.SegmentoN.SegmentoB.Items[x].DescOcorrencia;
            FPagFor.Registro0.Aviso.Last.Segmento        := 'N';
            FPagFor.Registro0.Aviso.Last.SegmentoFilho   := 'B';
            FPagFor.Registro0.Aviso.Last.SeuNumero       := FPagFor.Lote.Last.SegmentoN567.Last.SegmentoN.SeuNumero;
          end;
        end;
      end;
  end;
end;

procedure TPagForR.LerSegmentoN8(I: Integer);
var
  mok:boolean;
  x:integer;
begin
  if ((Copy(FArquivoTXT.Strings[i], 8, 1) + Copy(FArquivoTXT.Strings[i], 14, 1)) <> '3N') then
    Exit;

  if FPagFor.Geral.Banco = pagItau then exit; // Nao encontrei regsitro N8 para o Itau

  if (FPagFor.Geral.Banco <> pagItau) then // Teste para sair se for outro Banco
    Exit;

  case FPagFor.Geral.Banco of
    pagItau:
      begin

         FPagFor.Lote.Last.SegmentoN8.New;
         FPagFor.Lote.Last.SegmentoN8.Last.SegmentoN.CodMovimento := StrToInMovimento(mOk, Copy(FArquivoTXT.Strings[i], 0, 0));

        FPagFor.Lote.Last.SegmentoN8.Last.Receita                    := StrToInt(Copy(FArquivoTXT.Strings[i], 0, 0));
        FPagFor.Lote.Last.SegmentoN8.Last.TipoContribuinte           := StrToTpInscricao(mOk, Copy(FArquivoTXT.Strings[i], 0, 0));
        FPagFor.Lote.Last.SegmentoN8.Last.idContribuinte             := Copy(FArquivoTXT.Strings[i], 0, 0);
        FPagFor.Lote.Last.SegmentoN8.Last.InscEst                    := Copy(FArquivoTXT.Strings[i], 0, 0);
        FPagFor.Lote.Last.SegmentoN8.Last.Origem                     := Copy(FArquivoTXT.Strings[i], 0, 0);
        FPagFor.Lote.Last.SegmentoN8.Last.ValorPrincipal             := StrToInt(Copy(FArquivoTXT.Strings[i], 0, 0)) / 100;
        FPagFor.Lote.Last.SegmentoN8.Last.AtualizacaoMonetaria       := StrToInt(Copy(FArquivoTXT.Strings[i], 0, 0)) / 100;
        FPagFor.Lote.Last.SegmentoN8.Last.Mora                       := StrToInt(Copy(FArquivoTXT.Strings[i], 0, 0)) / 100;
        FPagFor.Lote.Last.SegmentoN8.Last.Multa                      := StrToInt(Copy(FArquivoTXT.Strings[i], 0, 0)) / 100;
        FPagFor.Lote.Last.SegmentoN8.Last.SegmentoN.ValorPagamento   := StrToInt(Copy(FArquivoTXT.Strings[i], 0, 0)) / 100;
        FPagFor.Lote.Last.SegmentoN8.Last.DataVencimento             := StringToDateTime(Copy(FArquivoTXT.Strings[i], 0, 2)+'/'+Copy(FArquivoTXT.Strings[i], 0, 2)+'/'+Copy(FArquivoTXT.Strings[i], 0, 4));
        FPagFor.Lote.Last.SegmentoN8.Last.SegmentoN.DataPagamento    := StringToDateTime(Copy(FArquivoTXT.Strings[i], 0, 2)+'/'+Copy(FArquivoTXT.Strings[i], 0, 2)+'/'+Copy(FArquivoTXT.Strings[i], 0, 4));
        FPagFor.Lote.Last.SegmentoN8.Last.PeriodoParcela             := StrToInt(Copy(FArquivoTXT.Strings[i], 0, 0));
        FPagFor.Lote.Last.SegmentoN8.Last.SegmentoN.NomeContribuinte := Copy(FArquivoTXT.Strings[i], 0, 0);
        FPagFor.Lote.Last.SegmentoN8.Last.SegmentoN.SeuNumero        := Copy(FArquivoTXT.Strings[i], 0, 0);

        FPagFor.Lote.Last.SegmentoN8.Last.SegmentoN.CodOcorrencia  := Trim(Copy(FArquivoTXT.Strings[i], 231, 10));
        FPagFor.Lote.Last.SegmentoN8.Last.SegmentoN.DescOcorrencia := DescricaoRetornoItau(FPagFor.Lote.Last.SegmentoN8.Last.SegmentoN.CodOcorrencia);

        if POS(FPagFor.Lote.Last.SegmentoN8.Last.SegmentoN.CodOcorrencia, PAGAMENTO_LIBERADO_AVISO) = 0 then
        begin
          FPagFor.Registro0.Aviso.New;
          FPagFor.Registro0.Aviso.Last.CodigoRetorno   := FPagFor.Lote.Last.SegmentoN8.Last.SegmentoN.CodOcorrencia;
          FPagFor.Registro0.Aviso.Last.MensagemRetorno := FPagFor.Lote.Last.SegmentoN8.Last.SegmentoN.DescOcorrencia;
          FPagFor.Registro0.Aviso.Last.Segmento        := 'N';
          FPagFor.Registro0.Aviso.Last.SegmentoFilho   := '';
          FPagFor.Registro0.Aviso.Last.SeuNumero       := FPagFor.Lote.Last.SegmentoN8.Last.SegmentoN.SeuNumero;
        end;
      end
  end;

  {Adicionais segmento N}
  LerSegmentoB(FPagFor.Lote.Last.SegmentoN8.Last.SegmentoN.SegmentoB, i);
  LerSegmentoW(FPagFor.Lote.Last.SegmentoN8.Last.SegmentoN.SegmentoW, i);
  LerSegmentoZ(FPagFor.Lote.Last.SegmentoN8.Last.SegmentoN.SegmentoZ, i);

  case FPagFor.Geral.Banco of
    pagItau:
      begin
        for x := 0 to FPagFor.Lote.Last.SegmentoN8.Last.SegmentoN.SegmentoB.Count - 1 do
        begin
          if POS(FPagFor.Lote.Last.SegmentoN8.Last.SegmentoN.SegmentoB.Items[x].CodOcorrencia, PAGAMENTO_LIBERADO_AVISO) = 0 then
          begin
            FPagFor.Registro0.Aviso.New;
            FPagFor.Registro0.Aviso.Last.CodigoRetorno   := FPagFor.Lote.Last.SegmentoN8.Last.SegmentoN.SegmentoB.Items[x].CodOcorrencia;
            FPagFor.Registro0.Aviso.Last.MensagemRetorno := FPagFor.Lote.Last.SegmentoN8.Last.SegmentoN.SegmentoB.Items[x].DescOcorrencia;
            FPagFor.Registro0.Aviso.Last.Segmento        := 'N';
            FPagFor.Registro0.Aviso.Last.SegmentoFilho   := 'B';
            FPagFor.Registro0.Aviso.Last.SeuNumero       := FPagFor.Lote.Last.SegmentoN8.Last.SegmentoN.SeuNumero;
          end;
        end;
      end;
  end;
end;

procedure TPagForR.LerSegmentoN9(I: Integer);
var
  mOk:boolean;
  x:integer;
begin
  if ((Copy(FArquivoTXT.Strings[i], 8, 1) + Copy(FArquivoTXT.Strings[i], 14, 1)) <> '3N') then
    Exit;

  if (FPagFor.Geral.Banco = pagItau) and (Copy(FArquivoTXT.Strings[i], 18, 2) <> '11') then // Só processa se for FGTS
    Exit;

  if (FPagFor.Geral.Banco <> pagItau) then // Teste para sair se for outro Banco
    Exit;

  case FPagFor.Geral.Banco of
    pagItau:
      begin

        FPagFor.Lote.Last.SegmentoN9.New;
        FPagFor.Lote.Last.SegmentoN9.Last.SegmentoN.CodMovimento := StrToInMovimento(mOk, Copy(FArquivoTXT.Strings[i], 0, 0));

        FPagFor.Lote.Last.SegmentoN9.Last.Receita                    := StrToInt(Copy(FArquivoTXT.Strings[i], 0, 0));

        if FArquivoTXT.Strings[i] = '1' then // Nesse segmento, 1 = CNPJ e 2 = CEI
          FPagFor.Lote.Last.SegmentoN9.Last.TipoContribuinte := tiCNPJ
        else
          FPagFor.Lote.Last.SegmentoN9.Last.TipoContribuinte := tiCPF;

        FPagFor.Lote.Last.SegmentoN9.Last.TipoContribuinte           := StrToTpInscricao(mOk, Copy(FArquivoTXT.Strings[i], 0, 0));
        FPagFor.Lote.Last.SegmentoN9.Last.idContribuinte             := Copy(FArquivoTXT.Strings[i], 0, 0);
        FPagFor.Lote.Last.SegmentoN9.Last.CodigoBarras               := Copy(FArquivoTXT.Strings[i], 0, 0);
        FPagFor.Lote.Last.SegmentoN9.Last.Identificador              := StrToInt(Copy(FArquivoTXT.Strings[i], 0, 0));
        FPagFor.Lote.Last.SegmentoN9.Last.Lacre                      := StrToInt(Copy(FArquivoTXT.Strings[i], 0, 0));
        FPagFor.Lote.Last.SegmentoN9.Last.LacreDigito                := StrToInt(Copy(FArquivoTXT.Strings[i], 0, 0));
        FPagFor.Lote.Last.SegmentoN9.Last.SegmentoN.NomeContribuinte := Copy(FArquivoTXT.Strings[i], 0, 0);
        FPagFor.Lote.Last.SegmentoN9.Last.SegmentoN.DataPagamento    := StringToDateTime(Copy(FArquivoTXT.Strings[i], 0, 2)+'/'+Copy(FArquivoTXT.Strings[i], 0, 2)+'/'+Copy(FArquivoTXT.Strings[i], 0, 4));
        FPagFor.Lote.Last.SegmentoN9.Last.SegmentoN.ValorPagamento   := StrToInt(Copy(FArquivoTXT.Strings[i], 0, 0)) / 100;
        FPagFor.Lote.Last.SegmentoN9.Last.SegmentoN.SeuNumero        := Copy(FArquivoTXT.Strings[i], 0, 0);

        FPagFor.Lote.Last.SegmentoN9.Last.SegmentoN.CodOcorrencia  := Trim(Copy(FArquivoTXT.Strings[i], 231, 10));
        FPagFor.Lote.Last.SegmentoN9.Last.SegmentoN.DescOcorrencia := DescricaoRetornoItau(FPagFor.Lote.Last.SegmentoN9.Last.SegmentoN.CodOcorrencia);

        if POS(FPagFor.Lote.Last.SegmentoN9.Last.SegmentoN.CodOcorrencia, PAGAMENTO_LIBERADO_AVISO) = 0 then
        begin
          FPagFor.Registro0.Aviso.New;
          FPagFor.Registro0.Aviso.Last.CodigoRetorno   := FPagFor.Lote.Last.SegmentoN9.Last.SegmentoN.CodOcorrencia;
          FPagFor.Registro0.Aviso.Last.MensagemRetorno := FPagFor.Lote.Last.SegmentoN9.Last.SegmentoN.DescOcorrencia;
          FPagFor.Registro0.Aviso.Last.Segmento        := 'N';
          FPagFor.Registro0.Aviso.Last.SegmentoFilho   := '';
          FPagFor.Registro0.Aviso.Last.SeuNumero       := FPagFor.Lote.Last.SegmentoN9.Last.SegmentoN.SeuNumero;
        end;
      end
  end;

  {Adicionais segmento N}
  LerSegmentoB(FPagFor.Lote.Last.SegmentoN9.Last.SegmentoN.SegmentoB, i);
  LerSegmentoW(FPagFor.Lote.Last.SegmentoN9.Last.SegmentoN.SegmentoW, i);
  LerSegmentoZ(FPagFor.Lote.Last.SegmentoN9.Last.SegmentoN.SegmentoZ, i);

  case FPagFor.Geral.Banco of
    pagItau:
      begin
        for x := 0 to FPagFor.Lote.Last.SegmentoN9.Last.SegmentoN.SegmentoB.Count - 1 do
        begin
          if POS(FPagFor.Lote.Last.SegmentoN9.Last.SegmentoN.SegmentoB.Items[x].CodOcorrencia, PAGAMENTO_LIBERADO_AVISO) = 0 then
          begin
            FPagFor.Registro0.Aviso.New;
            FPagFor.Registro0.Aviso.Last.CodigoRetorno   := FPagFor.Lote.Last.SegmentoN9.Last.SegmentoN.SegmentoB.Items[x].CodOcorrencia;
            FPagFor.Registro0.Aviso.Last.MensagemRetorno := FPagFor.Lote.Last.SegmentoN9.Last.SegmentoN.SegmentoB.Items[x].DescOcorrencia;
            FPagFor.Registro0.Aviso.Last.Segmento        := 'N';
            FPagFor.Registro0.Aviso.Last.SegmentoFilho   := 'B';
            FPagFor.Registro0.Aviso.Last.SeuNumero       := FPagFor.Lote.Last.SegmentoN9.Last.SegmentoN.SeuNumero;
          end;
        end;
      end;
  end;
end;

procedure TPagForR.LerSegmentoO(I: Integer);
begin
  if ((Copy(FArquivoTXT.Strings[i], 8, 1) + Copy(FArquivoTXT.Strings[i], 14, 1)) <> '3O') then
    Exit;

  FPagFor.Lote.Last.SegmentoO.New;
  FPagFor.Lote.Last.SegmentoO.Last.CodMovimento := TInstrucaoMovimento(StrToInt(Copy(FArquivoTXT.Strings[i], 15, 3)));
  case FPagFor.Geral.Banco of
    pagItau:
      begin
        FPagFor.Lote.Last.SegmentoO.Last.CodigoBarras       := Copy(FArquivoTXT.Strings[i], 18, 48);
        FPagFor.Lote.Last.SegmentoO.Last.NomeConcessionaria := Copy(FArquivoTXT.Strings[i], 66, 30);
        FPagFor.Lote.Last.SegmentoO.Last.DataVencimento     := StringToDateTime(Copy(FArquivoTXT.Strings[i], 96, 2)+'/'+Copy(FArquivoTXT.Strings[i], 98, 2)+'/'+Copy(FArquivoTXT.Strings[i], 100, 4));
        FPagFor.Lote.Last.SegmentoO.Last.QuantidadeMoeda    := StrToInt(Copy(FArquivoTXT.Strings[i], 107, 15)) / 100000000;
        FPagFor.Lote.Last.SegmentoO.Last.ValorPagamento     := StrToInt(Copy(FArquivoTXT.Strings[i], 122, 15)) / 100;
        FPagFor.Lote.Last.SegmentoO.Last.DataPagamento      := StringToDateTime(Copy(FArquivoTXT.Strings[i], 137, 2)+'/'+Copy(FArquivoTXT.Strings[i], 139, 2)+'/'+Copy(FArquivoTXT.Strings[i], 141, 4));
        FPagFor.Lote.Last.SegmentoO.Last.ValorPago          := StrToInt(Copy(FArquivoTXT.Strings[i], 145, 15))/100;
        FPagFor.Lote.Last.SegmentoO.Last.NotaFiscal         := StrToIntDef(Copy(FArquivoTXT.Strings[i], 163, 9), 0);
        FPagFor.Lote.Last.SegmentoO.Last.SeuNumero          := Copy(FArquivoTXT.Strings[i], 175, 20);
        FPagFor.Lote.Last.SegmentoO.Last.NossoNumero        := Copy(FArquivoTXT.Strings[i], 216, 15);
        FPagFor.Lote.Last.SegmentoO.Last.CodOcorrencia      := Trim(Copy(FArquivoTXT.Strings[i], 231, 10));
        FPagFor.Lote.Last.SegmentoO.Last.DescOcorrencia     := DescricaoRetornoItau(FPagFor.Lote.Last.SegmentoO.Last.CodOcorrencia);
      end;

    pagBancoDoBrasil,
    pagSicredi:
      begin
        FPagFor.Lote.Last.SegmentoO.Last.CodigoBarras       := Copy(FArquivoTXT.Strings[i], 18, 44);
        FPagFor.Lote.Last.SegmentoO.Last.NomeConcessionaria := Copy(FArquivoTXT.Strings[i], 62, 30);
        FPagFor.Lote.Last.SegmentoO.Last.DataVencimento     := StringToDateTime(Copy(FArquivoTXT.Strings[i], 92, 2)+'/'+Copy(FArquivoTXT.Strings[i], 94, 2)+'/'+Copy(FArquivoTXT.Strings[i], 96, 4));
        FPagFor.Lote.Last.SegmentoO.Last.DataPagamento      := StringToDateTime(Copy(FArquivoTXT.Strings[i], 100, 2)+'/'+Copy(FArquivoTXT.Strings[i], 102, 2)+'/'+Copy(FArquivoTXT.Strings[i], 104, 4));
        FPagFor.Lote.Last.SegmentoO.Last.ValorPagamento     := StrToInt(Copy(FArquivoTXT.Strings[i], 108, 15)) / 100;
        FPagFor.Lote.Last.SegmentoO.Last.SeuNumero          := Copy(FArquivoTXT.Strings[i], 123, 20);
        FPagFor.Lote.Last.SegmentoO.Last.NossoNumero        := Copy(FArquivoTXT.Strings[i], 143, 15);
        FPagFor.Lote.Last.SegmentoO.Last.CodOcorrencia      := Trim(Copy(FArquivoTXT.Strings[i], 231, 10));
        FPagFor.Lote.Last.SegmentoO.Last.DescOcorrencia     := DescricaoRetornoBancoDoBrasil(FPagFor.Lote.Last.SegmentoO.Last.CodOcorrencia);
      end;
  end;

  while pos((Copy(FArquivoTXT.Strings[i+1], 8, 1) + Copy(FArquivoTXT.Strings[i+1], 14, 1)), '3Z/') > 0 do
  begin
    inc(i); //próxima linha do txt a ser lida

    {opcionais segmento O}
    LerSegmentoZ(FPagFor.Lote.Last.SegmentoO.Last.SegmentoZ, i);
  end;
end;

procedure TPagForR.LerSegmentoP(I: Integer);
begin
 //
end;

procedure TPagForR.LerSegmentoQ(I: Integer);
begin
 //
end;

procedure TPagForR.LerSegmentoR(I: Integer);
begin
 //
end;

procedure TPagForR.LerSegmentoS(I: Integer);
begin
 //
end;

procedure TPagForR.LerSegmentoW(mSegmentoWList: TSegmentoWList; I:Integer);
begin
  if ((Copy(FArquivoTXT.Strings[i], 8, 1) + Copy(FArquivoTXT.Strings[i], 14, 1)) <> '3W') then
    Exit;

  FPagFor.Lote.Last.SegmentoW.New;
  case FPagFor.Geral.Banco of
    pagItau:
      begin
        FPagFor.Lote.Last.SegmentoW.Last.Informacoes1 := Copy(FArquivoTXT.Strings[i], 0, 0);
        FPagFor.Lote.Last.SegmentoW.Last.Informacoes2 := Copy(FArquivoTXT.Strings[i], 0, 0);
        FPagFor.Lote.Last.SegmentoW.Last.Informacoes3 := Copy(FArquivoTXT.Strings[i], 0, 0);
        FPagFor.Lote.Last.SegmentoW.Last.Informacoes4 := Copy(FArquivoTXT.Strings[i], 0, 0);
      end
  end;
end;

procedure TPagForR.LerSegmentoY(I: Integer);
begin
//
end;

procedure TPagForR.LerSegmentoZ(mSegmentoZList: TSegmentoZList; I:Integer);
begin
  if ((Copy(FArquivoTXT.Strings[i], 8, 1) + Copy(FArquivoTXT.Strings[i], 14, 1)) <> '3Z') then
    Exit;

  mSegmentoZList.New;
  mSegmentoZList.Last.Autenticacao := Copy(FArquivoTXT.Strings[i], 15, 64);
  mSegmentoZList.Last.SeuNumero    := Copy(FArquivoTXT.Strings[i], 79, 20);
  mSegmentoZList.Last.NossoNumero  := Copy(FArquivoTXT.Strings[i], 104, 15);
end;

function TPagForR.LerTXT(const ArquivoTXT: string): Boolean;
begin
  try
    FArquivoTXT.Text := ArquivoTXT;

    LerRegistro0;

    LerLote;

    LerRegistro9(FArquivoTXT.Count-1); {ultima linha é em branco}

    Result := True;
  except
    on E: Exception do
    begin
      raise Exception.Create('Não Foi Possível incluir Registros no Arquivo' + #13 + E.Message);
    end;
  end;
end;

end.

