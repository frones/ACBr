////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar BPe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml do BPe          //
//                                                                            //
//        site: www.projetocooperar.org                                       //
//       email: projetocooperar@zipmail.com.br                                //
//       forum: http://br.groups.yahoo.com/group/projeto_cooperar_nfe/        //
//     projeto: http://code.google.com/p/projetocooperar/                     //
//         svn: http://projetocooperar.googlecode.com/svn/trunk/              //
//                                                                            //
// Coordenação: (c) 2009 - Paulo Casagrande                                   //
//                                                                            //
//      Equipe: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
//      Versão: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
//     Licença: GNU Lesser General Public License (GNU LGPL)                  //
//                                                                            //
//              - Este programa é software livre; você pode redistribuí-lo    //
//              e/ou modificá-lo sob os termos da Licença Pública Geral GNU,  //
//              conforme publicada pela Free Software Foundation; tanto a     //
//              versão 2 da Licença como (a seu critério) qualquer versão     //
//              mais nova.                                                    //
//                                                                            //
//              - Este programa é distribuído na expectativa de ser útil,     //
//              mas SEM QUALQUER GARANTIA; sem mesmo a garantia implícita de  //
//              COMERCIALIZAÇÃO ou de ADEQUAÇÃO A QUALQUER PROPÓSITO EM       //
//              PARTICULAR. Consulte a Licença Pública Geral GNU para obter   //
//              mais detalhes. Você deve ter recebido uma cópia da Licença    //
//              Pública Geral GNU junto com este programa; se não, escreva    //
//              para a Free Software Foundation, Inc., 59 Temple Place,       //
//              Suite 330, Boston, MA - 02111-1307, USA ou consulte a         //
//              licença oficial em http://www.gnu.org/licenses/gpl.txt        //
//                                                                            //
//    Nota (1): - Esta  licença  não  concede  o  direito  de  uso  do nome   //
//              "PCN  -  Projeto  Cooperar  NFe", não  podendo o mesmo ser    //
//              utilizado sem previa autorização.                             //
//                                                                            //
//    Nota (2): - O uso integral (ou parcial) das units do projeto esta       //
//              condicionado a manutenção deste cabeçalho junto ao código     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

{*******************************************************************************
|* Historico
|*
|* 20/06/2017: Italo Jurisato Junior
|*  - Doação do componente para o Projeto ACBr
*******************************************************************************}

{$I ACBr.inc}

unit pcnConversaoBPe;

interface

uses
  SysUtils, StrUtils, Classes;

type
  TVersaoBPe = (ve100);

  TTipoBPe = (tbNormal, tbSubstituicao);

  TModalBPe = (moRodoviario, moAquaviario, moFerroviario);

  TSchemaBPe = (schErro, schBPe, schconsSitBPe, schconsStatServ, schEventoBPe,
                schdistDFeInt, schEnvEventoCancBPe, schEnvEventoNaoEmbBPe,
                schresBPe, schresEventoBPe, schprocBPe, schprocEventoBPe);

  TLayOutBPe = (LayBPeRecepcao, LayBPeRetRecepcao, LayBPeConsulta,
                LayBPeStatusServico, LayBPeEvento, LayBPeEventoAN, LayDistDFeInt);

  TStatusACBrBPe = (stIdleBPe, stBPeRecepcao, stBPeRetRecepcao, stBPeConsulta,
                    stBPeStatusServico, stBPeEvento, stBPeEmail,
                    stDistDFeInt, stEnvioWebService);

  TTipoSubstituicao = (tsRemarcacao, tsTransferencia, tsTransfRemarcacao);

  TTipoDocumento = (tdRG, tdTituloEleitor, tdPassaporte, tdCNH, tdOutros);

  TTipoViagem = (tvRegular, tvExtra);

  TTipoServico = (tsConvencionalComSanitario, tsConvencionalSemSanitario,
                  tsSemileito, tsLeitoComAr, tsLeitoSemAr, tsExecutivo,
                  tsSemiurbano, tsLongitudinal, tsTravessia);

  TTipoAcomodacao = (taAssento, taRede, taRedeComAr, taCabine, taOutros);

  TTipoTrecho = (ttNormal, ttTrechoInicial, ttConexao);

  TTipoVeiculo = (tvNenhum, tvMotocicleta, tvAutomovel, tvAtomovelComReboque,
                  tvCaminhonete, tvCaminhoneteComReboque, tvMicroOnibus,
                  tvVan, tvOnibus2ou3Eixos, tvOnibus4Eixos, tvCaminhao34,
                  tvCaminhaoToco, tvCaminhaoTruck, tvCarreta, tvBiTrem,
                  tvRodoTrem, tvRomeuJulieta, tvJamanta6Eixos, tvJamanta5Eixos,
                  tvJamanta4Eixos, tvTratorEsteira, tvPaMecanica, tvPatrol,
                  tvTratorPneuGrande, tvTratorPneuComReboque, tvPneuSemReboque,
                  tvCarroca, tvMobilete, tvBicicleta, tvPassageiro, tvOutros);

  TSitVeiculo = (svVazio, svCarregado, svNaoSeAplica);

  TTipoDesconto = (tdNenhum, tdTarifaPromocional, tdIdoso, tdCrianca, tdDeficiente,
                   tdEstudante, tdAnimalDomestico, tdAcordoColetivo,
                   tdProfissionalemDeslocamento, tdProfissionaldaEmpresa,
                   tdJovem, tdOutrosDesc);

  TTipoComponente = (tcTarifa, tcPedagio, tcTaxaEmbarque, tcSeguro, tcTRM,
                     tcSVI, tcOutros);

function StrToVersaoBPe(out ok: Boolean; const s: String): TVersaoBPe;
function VersaoBPeToStr(const t: TVersaoBPe): String;

function tpBPeToStr(const t: TTipoBPe): String;
function StrToTpBPe(out ok: Boolean; const s: String): TTipoBPe;

function ModalBPeToStr(const t: TModalBPe): String;
function StrToModalBPe(out ok: Boolean; const s: String): TModalBPe;

function DblToVersaoBPe(out ok: Boolean; const d: Real): TVersaoBPe;
function VersaoBPeToDbl(const t: TVersaoBPe): Real;

function LayOutToSchema(const t: TLayOutBPe): TSchemaBPe;

function SchemaBPeToStr(const t: TSchemaBPe): String;
function StrToSchemaBPe(out ok: Boolean; const s: String): TSchemaBPe;

function LayOutBPeToServico(const t: TLayOutBPe): String;
function ServicoToLayOutBPe(out ok: Boolean; const s: String): TLayOutBPe;

function tpSubstituicaoToStr(const t: TTipoSubstituicao): string;
function StrTotpSubstituicao(out ok: boolean; const s: string): TTipoSubstituicao;

function tpDocumentoToStr(const t: TTipoDocumento): string;
function StrTotpDocumento(out ok: boolean; const s: string): TTipoDocumento;

function tpViagemToStr(const t: TTipoViagem): string;
function StrTotpViagem(out ok: boolean; const s: string): TTipoViagem;

function tpServicoToStr(const t: TTipoServico): string;
function StrTotpServico(out ok: boolean; const s: string): TTipoServico;

function tpAcomodacaoToStr(const t: TTipoAcomodacao): string;
function StrTotpAcomodacao(out ok: boolean; const s: string): TTipoAcomodacao;

function tpTrechoToStr(const t: TTipoTrecho): string;
function StrTotpTrecho(out ok: boolean; const s: string): TTipoTrecho;

function tpVeiculoToStr(const t: TTipoVeiculo): string;
function StrTotpVeiculo(out ok: boolean; const s: string): TTipoVeiculo;

function SitVeiculoToStr(const t: TSitVeiculo): string;
function StrToSitVeiculo(out ok: boolean; const s: string): TSitVeiculo;

function tpDescontoToStr(const t: TTipoDesconto): string;
function StrTotpDesconto(out ok: boolean; const s: string): TTipoDesconto;

function tpComponenteToStr(const t: TTipoComponente): string;
function StrTotpComponente(out ok: boolean; const s: string): TTipoComponente;

implementation

uses
  pcnConversao, typinfo;

function StrToVersaoBPe(out ok: Boolean; const s: String): TVersaoBPe;
begin
  Result := StrToEnumerado(ok, s, ['1.00'], [ve100]);
end;

function VersaoBPeToStr(const t: TVersaoBPe): String;
begin
  Result := EnumeradoToStr(t, ['1.00'], [ve100]);
end;

function DblToVersaoBPe(out ok: Boolean; const d: Real): TVersaoBPe;
begin
  ok := True;

  if (d = 1.0)  then
    Result := ve100
  else
  begin
    Result := ve100;
    ok := False;
  end;
end;

function VersaoBPeToDbl(const t: TVersaoBPe): Real;
begin
  case t of
    ve100: Result := 1.00;
  else
    Result := 0;
  end;
end;

function tpBPeToStr(const t: TTipoBPe): String;
begin
  Result := EnumeradoToStr(t, ['1', '2'], [tbNormal, tbSubstituicao]);
end;

function StrToTpBPe(out ok: Boolean; const s: String): TTipoBPe;
begin
  Result := StrToEnumerado(ok, s, ['1', '2'], [tbNormal, tbSubstituicao]);
end;

function ModalBPeToStr(const t: TModalBPe): String;
begin
  result := EnumeradoToStr(t,
                           ['1', '3', '4'],
                           [moRodoviario, moAquaviario, moFerroviario]);
end;

function StrToModalBPe(out ok: Boolean; const s: String): TModalBPe;
begin
  result := StrToEnumerado(ok, s,
                           ['1', '3', '4'],
                           [moRodoviario, moAquaviario, moFerroviario]);
end;

function LayOutToSchema(const t: TLayOutBPe): TSchemaBPe;
begin
  case t of
    LayBPeRecepcao:      Result := schBPe;
    LayBPeConsulta:      Result := schconsSitBPe;
    LayBPeStatusServico: Result := schconsStatServ;
    LayBPeEvento,
    LayBPeEventoAN:      Result := schEventoBPe;
    LayDistDFeInt:       Result := schdistDFeInt;
  else
    Result := schErro;
  end;
end;

function SchemaBPeToStr(const t: TSchemaBPe): String;
begin
  Result := GetEnumName(TypeInfo(TSchemaBPe), Integer( t ) );
  Result := copy(Result, 4, Length(Result)); // Remove prefixo "sch"
end;

function StrToSchemaBPe(out ok: Boolean; const s: String): TSchemaBPe;
var
  P: Integer;
  SchemaStr: String;
begin
  P := pos('_',s);
  if p > 0 then
    SchemaStr := copy(s,1,P-1)
  else
    SchemaStr := s;

  if LeftStr(SchemaStr,3) <> 'sch' then
    SchemaStr := 'sch'+SchemaStr;

  Result := TSchemaBPe( GetEnumValue(TypeInfo(TSchemaBPe), SchemaStr ) );
end;

function LayOutBPeToServico(const t: TLayOutBPe): String;
begin
  Result := EnumeradoToStr(t,
    ['BPeRecepcao', 'BPeConsultaProtocolo', 'BPeStatusServico', 'BPeRetRecepcao',
     'BPeRecepcaoEvento', 'BPeRecepcaoEvento', 'BPeDistribuicaoDFe'],
    [ LayBPeRecepcao, LayBPeConsulta, LayBPeStatusServico, LayBPeRetRecepcao,
      LayBPeEvento, LayBPeEventoAN, LayDistDFeInt ] );
end;

function ServicoToLayOutBPe(out ok: Boolean; const s: String): TLayOutBPe;
begin
  Result := StrToEnumerado(ok, s,
    ['BPeRecepcao', 'BPeConsultaProtocolo', 'BPeStatusServico', 'BPeRetRecepcao',
     'BPeRecepcaoEvento', 'BPeRecepcaoEvento', 'BPeDistribuicaoDFe'],
    [ LayBPeRecepcao, LayBPeConsulta, LayBPeStatusServico, LayBPeRetRecepcao,
      LayBPeEvento, LayBPeEventoAN, LayDistDFeInt ] );
end;

function tpSubstituicaoToStr(const t: TTipoSubstituicao): string;
begin
  result := EnumeradoToStr(t,
                           ['1', '2', '3'],
                           [tsRemarcacao, tsTransferencia, tsTransfRemarcacao]);
end;

function StrTotpSubstituicao(out ok: boolean; const s: string): TTipoSubstituicao;
begin
  result := StrToEnumerado(ok, s,
                           ['1', '2', '3'],
                           [tsRemarcacao, tsTransferencia, tsTransfRemarcacao]);
end;

function tpDocumentoToStr(const t: TTipoDocumento): string;
begin
  result := EnumeradoToStr(t,
                           ['1', '2', '3', '4', '5'],
                           [tdRG, tdTituloEleitor, tdPassaporte, tdCNH, tdOutros]);
end;

function StrTotpDocumento(out ok: boolean; const s: string): TTipoDocumento;
begin
  result := StrToEnumerado(ok, s,
                           ['1', '2', '3', '4', '5'],
                           [tdRG, tdTituloEleitor, tdPassaporte, tdCNH, tdOutros]);
end;

function tpViagemToStr(const t: TTipoViagem): string;
begin
  result := EnumeradoToStr(t,
                           ['00', '01'],
                           [tvRegular, tvExtra]);
end;

function StrTotpViagem(out ok: boolean; const s: string): TTipoViagem;
begin
  result := StrToEnumerado(ok, s,
                           ['00', '01'],
                           [tvRegular, tvExtra]);
end;

function tpServicoToStr(const t: TTipoServico): string;
begin
  result := EnumeradoToStr(t,
                           ['1', '2', '3', '4', '5', '6', '7', '8', '9'],
                           [tsConvencionalComSanitario, tsConvencionalSemSanitario,
                            tsSemileito, tsLeitoComAr, tsLeitoSemAr, tsExecutivo,
                            tsSemiurbano, tsLongitudinal, tsTravessia]);
end;

function StrTotpServico(out ok: boolean; const s: string): TTipoServico;
begin
  result := StrToEnumerado(ok, s,
                           ['1', '2', '3', '4', '5', '6', '7', '8', '9'],
                           [tsConvencionalComSanitario, tsConvencionalSemSanitario,
                            tsSemileito, tsLeitoComAr, tsLeitoSemAr, tsExecutivo,
                            tsSemiurbano, tsLongitudinal, tsTravessia]);
end;

function tpAcomodacaoToStr(const t: TTipoAcomodacao): string;
begin
  result := EnumeradoToStr(t,
                           ['1', '2', '3', '4', '5'],
                           [taAssento, taRede, taRedeComAr, taCabine, taOutros]);
end;

function StrTotpAcomodacao(out ok: boolean; const s: string): TTipoAcomodacao;
begin
  result := StrToEnumerado(ok, s,
                           ['1', '2', '3', '4', '5'],
                           [taAssento, taRede, taRedeComAr, taCabine, taOutros]);
end;

function tpTrechoToStr(const t: TTipoTrecho): string;
begin
  result := EnumeradoToStr(t,
                           ['1', '2', '3'],
                           [ttNormal, ttTrechoInicial, ttConexao]);
end;

function StrTotpTrecho(out ok: boolean; const s: string): TTipoTrecho;
begin
  result := StrToEnumerado(ok, s,
                           ['1', '2', '3'],
                           [ttNormal, ttTrechoInicial, ttConexao]);
end;

function tpVeiculoToStr(const t: TTipoVeiculo): string;
begin
  result := EnumeradoToStr(t,
                           ['00', '01', '02', '03', '04', '05', '06', '07', '08',
                            '09', '10', '11', '12', '13', '14', '15', '16', '17',
                            '18', '19', '20', '21', '22', '23', '24', '25', '26',
                            '27', '28', '29', '99'],
                           [tvNenhum, tvMotocicleta, tvAutomovel, tvAtomovelComReboque,
                            tvCaminhonete, tvCaminhoneteComReboque, tvMicroOnibus,
                            tvVan, tvOnibus2ou3Eixos, tvOnibus4Eixos, tvCaminhao34,
                            tvCaminhaoToco, tvCaminhaoTruck, tvCarreta, tvBiTrem,
                            tvRodoTrem, tvRomeuJulieta, tvJamanta6Eixos, tvJamanta5Eixos,
                            tvJamanta4Eixos, tvTratorEsteira, tvPaMecanica, tvPatrol,
                            tvTratorPneuGrande, tvTratorPneuComReboque, tvPneuSemReboque,
                            tvCarroca, tvMobilete, tvBicicleta, tvPassageiro, tvOutros]);
end;

function StrTotpVeiculo(out ok: boolean; const s: string): TTipoVeiculo;
begin
  result := StrToEnumerado(ok, s,
                           ['00', '01', '02', '03', '04', '05', '06', '07', '08',
                            '09', '10', '11', '12', '13', '14', '15', '16', '17',
                            '18', '19', '20', '21', '22', '23', '24', '25', '26',
                            '27', '28', '29', '99'],
                           [tvNenhum, tvMotocicleta, tvAutomovel, tvAtomovelComReboque,
                            tvCaminhonete, tvCaminhoneteComReboque, tvMicroOnibus,
                            tvVan, tvOnibus2ou3Eixos, tvOnibus4Eixos, tvCaminhao34,
                            tvCaminhaoToco, tvCaminhaoTruck, tvCarreta, tvBiTrem,
                            tvRodoTrem, tvRomeuJulieta, tvJamanta6Eixos, tvJamanta5Eixos,
                            tvJamanta4Eixos, tvTratorEsteira, tvPaMecanica, tvPatrol,
                            tvTratorPneuGrande, tvTratorPneuComReboque, tvPneuSemReboque,
                            tvCarroca, tvMobilete, tvBicicleta, tvPassageiro, tvOutros]);
end;

function SitVeiculoToStr(const t: TSitVeiculo): string;
begin
  result := EnumeradoToStr(t,
                           ['01', '02', '03'],
                           [svVazio, svCarregado, svNaoSeAplica]);
end;

function StrToSitVeiculo(out ok: boolean; const s: string): TSitVeiculo;
begin
  result := StrToEnumerado(ok, s,
                           ['01', '02', '03'],
                           [svVazio, svCarregado, svNaoSeAplica]);
end;

function tpDescontoToStr(const t: TTipoDesconto): string;
begin
  result := EnumeradoToStr(t,
                           ['00', '01', '02', '03', '04', '05', '06', '07', '08',
                            '09', '10', '99'],
                           [tdNenhum, tdTarifaPromocional, tdIdoso, tdCrianca,
                            tdDeficiente, tdEstudante, tdAnimalDomestico,
                            tdAcordoColetivo, tdProfissionalemDeslocamento,
                            tdProfissionaldaEmpresa, tdJovem, tdOutrosDesc]);
end;

function StrTotpDesconto(out ok: boolean; const s: string): TTipoDesconto;
begin
  result := StrToEnumerado(ok, s,
                           ['00', '01', '02', '03', '04', '05', '06', '07', '08',
                            '09', '10', '99'],
                           [tdNenhum, tdTarifaPromocional, tdIdoso, tdCrianca,
                            tdDeficiente, tdEstudante, tdAnimalDomestico,
                            tdAcordoColetivo, tdProfissionalemDeslocamento,
                            tdProfissionaldaEmpresa, tdJovem, tdOutrosDesc]);
end;

function tpComponenteToStr(const t: TTipoComponente): string;
begin
  result := EnumeradoToStr(t,
                           ['01', '02', '03', '04', '05', '06', '99'],
                           [tcTarifa, tcPedagio, tcTaxaEmbarque, tcSeguro, tcTRM,
                            tcSVI, tcOutros]);
end;

function StrTotpComponente(out ok: boolean; const s: string): TTipoComponente;
begin
  result := StrToEnumerado(ok, s,
                           ['01', '02', '03', '04', '05', '06', '99'],
                           [tcTarifa, tcPedagio, tcTaxaEmbarque, tcSeguro, tcTRM,
                            tcSVI, tcOutros]);
end;

end.

