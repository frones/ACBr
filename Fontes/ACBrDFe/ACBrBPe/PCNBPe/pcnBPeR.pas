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

unit pcnBPeR;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnLeitor, pcnBPe;

type

  TBPeR = class(TObject)
  private
    FLeitor: TLeitor;
    FBPe: TBPe;
  public
    constructor Create(AOwner: TBPe);
    destructor Destroy; override;
    function LerXml: Boolean;
    property Leitor: TLeitor read FLeitor write FLeitor;
    property BPe: TBPe       read FBPe    write FBPe;
  end;

implementation

uses
  pcnConversaoBPe,
  ACBrUtil;

{ TBPeR }

constructor TBPeR.Create(AOwner: TBPe);
begin
  inherited Create;
  FLeitor := TLeitor.Create;
  FBPe    := AOwner;
end;

destructor TBPeR.Destroy;
begin
  FLeitor.Free;
  inherited Destroy;
end;

function TBPeR.LerXml: Boolean;
var
  ok: Boolean;
  i: Integer;
  VersaoInfBPe: AnsiString;
  Aspas: String;
begin
  Leitor.Grupo := Leitor.Arquivo;

  if Pos('versao="', Leitor.Arquivo) <> 0 then
    Aspas := '"'
   else
    Aspas := '''';

  BPe.infBPe.Id := Leitor.rAtributo('Id=', 'infBPe');
  if OnlyNumber(BPe.infBPe.Id) = '' then
    raise Exception.Create('Não encontrei o atributo: Id');

  VersaoInfBPe := Leitor.rAtributo('versao=', 'infBPe');
  if StringToFloatDef(VersaoInfBPe, -1) = -1 then
    raise Exception.Create('Não encontrei o atributo: versao');

  BPe.infBPe.Versao := StringToFloat(VersaoInfBPe);

  if Leitor.rExtrai(1, 'ide') <> '' then
  begin
    BPe.ide.cUF    := Leitor.rCampo(tcInt, 'cUF');
    BPe.Ide.tpAmb  := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
    BPe.ide.modelo := Leitor.rCampo(tcInt, 'mod');
    BPe.ide.serie  := Leitor.rCampo(tcInt, 'serie');
    BPe.ide.nBP    := Leitor.rCampo(tcInt, 'nBP');
    BPe.ide.cBP    := Leitor.rCampo(tcInt, 'cBP');

    if BPe.ide.cBP = 0 then
      BPe.ide.cBP := -2;

    BPe.Ide.cDV    := Leitor.rCampo(tcInt, 'cDV');

    BPe.Ide.modal  := StrToModalBPe(Ok, Leitor.rCampo(tcStr, 'modal'));
    BPe.ide.dhEmi   := Leitor.rCampo(tcDatHor, 'dhEmi');
    BPe.Ide.tpEmis  := StrToTpEmis(ok, Leitor.rCampo(tcStr, 'tpEmis'));
    BPe.Ide.verProc := Leitor.rCampo(tcStr, 'verProc');
    BPe.ide.tpBPe   := StrToTpBPe(ok, Leitor.rCampo(tcStr, 'tpBPe'));
    BPe.ide.indPres := StrToPresencaComprador(ok, Leitor.rCampo(tcStr, 'indPres'));
    BPe.Ide.UFIni   := Leitor.rCampo(tcStr, 'UFIni');
    BPe.Ide.cMunIni := Leitor.rCampo(tcInt, 'cMunIni');
    BPe.Ide.UFFim   := Leitor.rCampo(tcStr, 'UFFim');
    BPe.Ide.cMunFim := Leitor.rCampo(tcInt, 'cMunFim');
    BPe.Ide.dhCont  := Leitor.rCampo(tcDatHor, 'dhCont');
    BPe.Ide.xJust   := Leitor.rCampo(tcStr, 'xJust');
  end;

  if Leitor.rExtrai(1, 'emit') <> '' then
  begin
    BPe.Emit.CNPJ  := Leitor.rCampo(tcStr, 'CNPJ');
    BPe.Emit.IE    := Leitor.rCampo(tcStr, 'IE');
    BPe.Emit.IEST  := Leitor.rCampo(tcStr, 'IEST');
    BPe.Emit.xNome := Leitor.rCampo(tcStr, 'xNome');
    BPe.Emit.xFant := Leitor.rCampo(tcStr, 'xFant');
    BPe.Emit.IM    := Leitor.rCampo(tcStr, 'IM');
    BPe.Emit.CNAE  := Leitor.rCampo(tcStr, 'CNAE');
    BPe.Emit.CRT   := StrToCRT(ok, Leitor.rCampo(tcStr, 'CRT'));
    BPe.Emit.TAR   := Leitor.rCampo(tcStr, 'TAR');

    if Leitor.rExtrai(2, 'enderEmit') <> '' then
    begin
      BPe.Emit.enderEmit.xLgr    := Leitor.rCampo(tcStr, 'xLgr');
      BPe.Emit.enderEmit.nro     := Leitor.rCampo(tcStr, 'nro');
      BPe.Emit.enderEmit.xCpl    := Leitor.rCampo(tcStr, 'xCpl');
      BPe.Emit.enderEmit.xBairro := Leitor.rCampo(tcStr, 'xBairro');
      BPe.Emit.EnderEmit.cMun    := Leitor.rCampo(tcInt, 'cMun');
      BPe.Emit.enderEmit.xMun    := Leitor.rCampo(tcStr, 'xMun');
      BPe.Emit.enderEmit.CEP     := Leitor.rCampo(tcInt, 'CEP');
      BPe.Emit.enderEmit.UF      := Leitor.rCampo(tcStr, 'UF');
      BPe.Emit.enderEmit.fone    := Leitor.rCampo(tcStr, 'fone');
      BPe.Emit.enderEmit.Email   := Leitor.rCampo(tcStr, 'email');
    end;
  end;

  if Leitor.rExtrai(1, 'comp') <> '' then
  begin
    BPe.Comp.xNome         := Leitor.rCampo(tcStr, 'xNome');
    BPe.Comp.CNPJCPF       := Leitor.rCampoCNPJCPF;
    BPe.Comp.idEstrangeiro := Leitor.rCampo(tcStr, 'idEstrangeiro');
    BPe.Comp.IE            := Leitor.rCampo(tcStr, 'IE');

    if Leitor.rExtrai(2, 'enderComp') <> '' then
    begin
      BPe.Comp.enderComp.xLgr    := Leitor.rCampo(tcStr, 'xLgr');
      BPe.Comp.enderComp.nro     := Leitor.rCampo(tcStr, 'nro');
      BPe.Comp.enderComp.xCpl    := Leitor.rCampo(tcStr, 'xCpl');
      BPe.Comp.enderComp.xBairro := Leitor.rCampo(tcStr, 'xBairro');
      BPe.Comp.EnderComp.cMun    := Leitor.rCampo(tcInt, 'cMun');
      BPe.Comp.enderComp.xMun    := Leitor.rCampo(tcStr, 'xMun');
      BPe.Comp.enderComp.CEP     := Leitor.rCampo(tcInt, 'CEP');
      BPe.Comp.enderComp.UF      := Leitor.rCampo(tcStr, 'UF');
      BPe.Comp.enderComp.cPais   := Leitor.rCampo(tcInt, 'cPais');

      if BPe.Comp.enderComp.cPais = 0 then
        BPe.Comp.enderComp.cPais := 1058;

      BPe.Comp.enderComp.xPais   := Leitor.rCampo(tcStr, 'xPais');

      if BPe.Comp.enderComp.xPais = '' then
        BPe.Comp.enderComp.xPais := 'BRASIL';

      BPe.Comp.enderComp.fone    := Leitor.rCampo(tcStr, 'fone');
      BPe.Comp.enderComp.Email   := Leitor.rCampo(tcStr, 'email');
    end;
  end;

  if Leitor.rExtrai(1, 'agencia') <> '' then
  begin
    BPe.Agencia.xNome := Leitor.rCampo(tcStr, 'xNome');
    BPe.Agencia.CNPJ  := Leitor.rCampo(tcStr, 'CNPJ');

    if Leitor.rExtrai(2, 'enderAgencia') <> '' then
    begin
      BPe.Agencia.enderAgencia.xLgr    := Leitor.rCampo(tcStr, 'xLgr');
      BPe.Agencia.enderAgencia.nro     := Leitor.rCampo(tcStr, 'nro');
      BPe.Agencia.enderAgencia.xCpl    := Leitor.rCampo(tcStr, 'xCpl');
      BPe.Agencia.enderAgencia.xBairro := Leitor.rCampo(tcStr, 'xBairro');
      BPe.Agencia.EnderAgencia.cMun    := Leitor.rCampo(tcInt, 'cMun');
      BPe.Agencia.enderAgencia.xMun    := Leitor.rCampo(tcStr, 'xMun');
      BPe.Agencia.enderAgencia.CEP     := Leitor.rCampo(tcInt, 'CEP');
      BPe.Agencia.enderAgencia.UF      := Leitor.rCampo(tcStr, 'UF');
      BPe.Agencia.enderAgencia.cPais   := Leitor.rCampo(tcInt, 'cPais');

      if BPe.Agencia.enderAgencia.cPais = 0 then
        BPe.Agencia.enderAgencia.cPais := 1058;

      BPe.Agencia.enderAgencia.xPais   := Leitor.rCampo(tcStr, 'xPais');

      if BPe.Agencia.enderAgencia.xPais = '' then
        BPe.Agencia.enderAgencia.xPais := 'BRASIL';

      BPe.Agencia.enderAgencia.fone    := Leitor.rCampo(tcStr, 'fone');
      BPe.Agencia.enderAgencia.Email   := Leitor.rCampo(tcStr, 'email');
    end;
  end;

  if Leitor.rExtrai(1, 'infBPeSub') <> '' then
  begin
    BPe.infBPeSub.chBPe := Leitor.rCampo(tcStr, 'chBPe');
    BPe.infBPeSub.tpSub := StrTotpSubstituicao(ok, Leitor.rCampo(tcStr, 'tpSub'));
  end;

  if Leitor.rExtrai(1, 'infPassagem') <> '' then
  begin
    BPe.infPassagem.cLocOrig   := Leitor.rCampo(tcStr, 'cLocOrig');
    BPe.infPassagem.xLocOrig   := Leitor.rCampo(tcStr, 'xLocOrig');
    BPe.infPassagem.cLocDest   := Leitor.rCampo(tcStr, 'cLocDest');
    BPe.infPassagem.xLocDest   := Leitor.rCampo(tcStr, 'xLocDest');
    BPe.infPassagem.dhEmb      := Leitor.rCampo(tcDatHor, 'dhEmb');
//    BPe.infPassagem.dhValidade := Leitor.rCampo(tcDatHor, 'dhValidade');
    BPe.infPassagem.dhValidade := Leitor.rCampo(tcDatHor, 'dhValidade');

    if Leitor.rExtrai(2, 'infPassageiro') <> '' then
    begin
      BPe.infPassagem.infPassageiro.xNome := Leitor.rCampo(tcStr, 'xNome');
      BPe.infPassagem.infPassageiro.CPF   := Leitor.rCampo(tcStr, 'CPF');
      BPe.infPassagem.infPassageiro.tpDoc := StrTotpDocumento(ok, Leitor.rCampo(tcStr, 'tpDoc'));
      BPe.infPassagem.infPassageiro.nDoc  := Leitor.rCampo(tcStr, 'nDoc');
      BPe.infPassagem.infPassageiro.xDoc  := Leitor.rCampo(tcStr, 'xDoc');
      BPe.infPassagem.infPassageiro.dNasc := Leitor.rCampo(tcDat, 'dNasc');
      BPe.infPassagem.infPassageiro.Fone  := Leitor.rCampo(tcStr, 'fone');
      BPe.infPassagem.infPassageiro.Email := Leitor.rCampo(tcStr, 'email');
    end;
  end;

  i := 0;
  BPe.infViagem.Clear;
  while Leitor.rExtrai(1, 'infViagem', '', i + 1) <> '' do
  begin
    BPe.infViagem.Add;
    BPe.infViagem[i].cPercurso    := Leitor.rCampo(tcStr, 'cPercurso');
    BPe.infViagem[i].xPercurso    := Leitor.rCampo(tcStr, 'xPercurso');
    BPe.infViagem[i].tpViagem     := StrTotpViagem(ok, Leitor.rCampo(tcStr, 'tpViagem'));
    BPe.infViagem[i].tpServ       := StrTotpServico(ok, Leitor.rCampo(tcStr, 'tpServ'));
    BPe.infViagem[i].tpAcomodacao := StrTotpAcomodacao(ok, Leitor.rCampo(tcStr, 'tpAcomodacao'));
    BPe.infViagem[i].tpTrecho     := StrTotpTrecho(ok, Leitor.rCampo(tcStr, 'tpTrecho'));
    BPe.infViagem[i].dhViagem     := Leitor.rCampo(tcDatHor, 'dhViagem');
    BPe.infViagem[i].dhConexao    := Leitor.rCampo(tcDatHor, 'dhConexao');
    BPe.infViagem[i].Prefixo      := Leitor.rCampo(tcStr, 'prefixo');
    BPe.infViagem[i].Poltrona     := Leitor.rCampo(tcInt, 'poltrona');
    BPe.infViagem[i].Plataforma   := Leitor.rCampo(tcStr, 'plataforma');

    if Leitor.rExtrai(2, 'infTravessia') <> '' then
    begin
      BPe.infViagem[i].infTravessia.tpVeiculo  := StrTotpVeiculo(ok, Leitor.rCampo(tcStr, 'tpVeiculo'));
      BPe.infViagem[i].infTravessia.sitVeiculo := StrToSitVeiculo(ok, Leitor.rCampo(tcStr, 'sitVeiculo'));
    end;

    inc(i);
  end;

  if Leitor.rExtrai(1, 'infValorBPe') <> '' then
  begin
    BPe.infValorBPe.vBP        := Leitor.rCampo(tcDe2, 'vBP');
    BPe.infValorBPe.vDesconto  := Leitor.rCampo(tcDe2, 'vDesconto');
    BPe.infValorBPe.vPgto      := Leitor.rCampo(tcDe2, 'vPgto');
    BPe.infValorBPe.vTroco     := Leitor.rCampo(tcDe2, 'vTroco');
    BPe.infValorBPe.tpDesconto := StrTotpDesconto(ok, Leitor.rCampo(tcStr, 'tpDesconto'));
    BPe.infValorBPe.xDesconto  := Leitor.rCampo(tcStr, 'xDesconto');
    BPe.infValorBPe.cDesconto  := Leitor.rCampo(tcStr, 'cDesconto');

    i := 0;
    BPe.infValorBPe.Comp.Clear;
    while Leitor.rExtrai(2, 'Comp', '', i + 1) <> '' do
    begin
      BPe.infValorBPe.Comp.Add;
      BPe.infValorBPe.Comp[i].tpComp := StrTotpComponente(ok, Leitor.rCampo(tcStr, 'tpComp'));
      BPe.infValorBPe.Comp[i].vComp  := Leitor.rCampo(tcDe2, 'vComp');

      inc(i);
    end;
  end;

  if Leitor.rExtrai(1, 'imp') <> '' then
  begin
    BPe.Imp.vTotTrib   := Leitor.rCampo(tcDe2, 'vTotTrib');
    BPe.Imp.infAdFisco := Leitor.rCampo(tcStr, 'infAdFisco');

    if Leitor.rExtrai(2, 'ICMS') <> '' then
    begin
      BPe.Imp.ICMS.CST           := StrToCSTICMS(ok, Leitor.rCampo(tcStr, 'CST'));
      BPe.Imp.ICMS.vBC           := Leitor.rCampo(tcDe2, 'vBC');
      BPe.Imp.ICMS.pICMS         := Leitor.rCampo(tcDe2, 'pICMS');
      BPe.Imp.ICMS.vICMS         := Leitor.rCampo(tcDe2, 'vICMS');
      BPe.Imp.ICMS.pRedBC        := Leitor.rCampo(tcDe2, 'pRedBC');
      BPe.Imp.ICMS.vCred         := Leitor.rCampo(tcDe2, 'vCred');
      BPe.Imp.ICMS.pRedBCOutraUF := Leitor.rCampo(tcDe2, 'pRedBCOutraUF');
      BPe.Imp.ICMS.vBCOutraUF    := Leitor.rCampo(tcDe2, 'vBCOutraUF');
      BPe.Imp.ICMS.pICMSOutraUF  := Leitor.rCampo(tcDe2, 'pICMSOutraUF');
      BPe.Imp.ICMS.vICMSOutraUF  := Leitor.rCampo(tcDe2, 'vICMSOutraUF');

      if Leitor.rExtrai(3, 'ICMSOutraUF') <> '' then
        BPe.Imp.ICMS.CST := cstICMSOutraUF;

      if Leitor.rExtrai(3, 'ICMSSN') <> '' then
        BPe.Imp.ICMS.CST := cstICMSSN;
    end;

    if Leitor.rExtrai(2, 'ICMSUFFim') <> '' then
    begin
      BPe.Imp.ICMSUFFim.vBCUFFim       := Leitor.rCampo(tcDe2, 'vBCUFFim');
      BPe.Imp.ICMSUFFim.pFCPUFFim      := Leitor.rCampo(tcDe2, 'pFCPUFFim');
      BPe.Imp.ICMSUFFim.pICMSUFFim     := Leitor.rCampo(tcDe2, 'pICMSUFFim');
      BPe.Imp.ICMSUFFim.pICMSInter     := Leitor.rCampo(tcDe2, 'pICMSInter');
      BPe.Imp.ICMSUFFim.pICMSInterPart := Leitor.rCampo(tcDe2, 'pICMSInterPart');
      BPe.Imp.ICMSUFFim.vFCPUFFim      := Leitor.rCampo(tcDe2, 'vFCPUFFim');
      BPe.Imp.ICMSUFFim.vICMSUFFim     := Leitor.rCampo(tcDe2, 'vICMSUFFim');
      BPe.Imp.ICMSUFFim.vICMSUFIni     := Leitor.rCampo(tcDe2, 'vICMSUFIni');
    end;
  end;

  i := 0;
  BPe.Pag.Clear;
  while Leitor.rExtrai(1, 'pag', '', i + 1) <> '' do
  begin
    BPe.Pag.Add;
    BPe.Pag[i].tPag    := StrToFormaPagamento(ok, Leitor.rCampo(tcStr, 'tPag'));
    BPe.Pag[i].xPag    := Leitor.rCampo(tcStr, 'xPag');
    BPe.Pag[i].nDocPag := Leitor.rCampo(tcStr, 'nDocPag');
    BPe.Pag[i].vPag    := Leitor.rCampo(tcDe2, 'vPag');

    BPe.Pag[i].tpIntegra := StrTotpIntegra(ok, Leitor.rCampo(tcStr, 'tpIntegra'));
    BPe.Pag[i].CNPJ      := Leitor.rCampo(tcStr, 'CNPJ');
    BPe.Pag[i].tBand     := StrToBandeiraCard(ok, Leitor.rCampo(tcStr, 'tBand'));
    BPe.Pag[i].xBand     := Leitor.rCampo(tcStr, 'xBand');
    BPe.Pag[i].cAut      := Leitor.rCampo(tcStr, 'cAut');
    BPe.Pag[i].nsuTrans  := Leitor.rCampo(tcStr, 'nsuTrans');
    BPe.Pag[i].nsuHost   := Leitor.rCampo(tcStr, 'nsuHost');
    BPe.Pag[i].nParcelas := Leitor.rCampo(tcInt, 'nParcelas');
    BPe.Pag[i].infAdCard := Leitor.rCampo(tcStr, 'infAdCard');

    inc(i);
  end;

  i := 0;
  BPe.autXML.Clear;
  while Leitor.rExtrai(1, 'autXML', '', i + 1) <> '' do
  begin
    BPe.autXML.Add;
    BPe.autXML[i].CNPJCPF := Leitor.rCampoCNPJCPF;;

    inc(i);
  end;

  if Leitor.rExtrai(1, 'infAdic') <> '' then
  begin
    BPe.InfAdic.infAdFisco := Leitor.rCampo(tcStr, 'infAdFisco');
    BPe.InfAdic.infCpl     := Leitor.rCampo(tcStr, 'infCpl');
  end;

  if Leitor.rExtrai(1, 'infRespTec') <> '' then
  begin
    BPe.infRespTec.CNPJ     := Leitor.rCampo(tcStr, 'CNPJ');
    BPe.infRespTec.xContato := Leitor.rCampo(tcStr, 'xContato');
    BPe.infRespTec.email    := Leitor.rCampo(tcStr, 'email');
    BPe.infRespTec.fone     := Leitor.rCampo(tcStr, 'fone');
    BPe.infRespTec.idCSRT   := Leitor.rCampo(tcInt, 'idCSRT');
    BPe.infRespTec.hashCSRT := Leitor.rCampo(tcStr, 'hashCSRT');
  end;

  leitor.Grupo := Leitor.Arquivo;

  BPe.signature.URI             := Leitor.rAtributo('Reference URI=');
  BPe.signature.DigestValue     := Leitor.rCampo(tcStr, 'DigestValue');
  BPe.signature.SignatureValue  := Leitor.rCampo(tcStr, 'SignatureValue');
  BPe.signature.X509Certificate := Leitor.rCampo(tcStr, 'X509Certificate');

  if Leitor.rExtrai(1, 'infBPeSupl') <> '' then
  begin
    BPe.infBPeSupl.qrCodBPe := Leitor.rCampo(tcStr, 'qrCodBPe');
    BPe.infBPeSupl.qrCodBPe := StringReplace(BPe.infBPeSupl.qrCodBPe, '<![CDATA[', '', []);
    BPe.infBPeSupl.qrCodBPe := StringReplace(BPe.infBPeSupl.qrCodBPe, ']]>', '', []);

    BPe.infBPeSupl.boardPassBPe := Leitor.rCampo(tcStr, 'boardPassBPe');
  end;

  if Leitor.rExtrai(1, 'protBPe') <> '' then
  begin
    BPe.procBPe.tpAmb    := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
    BPe.procBPe.verAplic := Leitor.rCampo(tcStr, 'verAplic');
    BPe.procBPe.chBPe    := Leitor.rCampo(tcStr, 'chBPe');
    BPe.procBPe.dhRecbto := Leitor.rCampo(tcDatHor, 'dhRecbto');
    BPe.procBPe.nProt    := Leitor.rCampo(tcStr, 'nProt');
    BPe.procBPe.digVal   := Leitor.rCampo(tcStr, 'digVal');
    BPe.procBPe.cStat    := Leitor.rCampo(tcInt, 'cStat');
    BPe.procBPe.xMotivo  := Leitor.rCampo(tcStr, 'xMotivo');
  end;

  Result := true;
end;

end.

