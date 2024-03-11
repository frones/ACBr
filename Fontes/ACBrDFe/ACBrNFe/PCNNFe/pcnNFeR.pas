////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar NFe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml da NFe          //
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

{******************************************************************************
|* Historico
|*
|* 24/09/2012: Italo Jurisato Junior
|*  - Alterações para funcionamento com NFC-e
******************************************************************************}

{$I ACBr.inc}

unit pcnNFeR;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnLeitor, pcnNFe;

type

  TNFeR = class(TPersistent)
  private
    FLeitor: TLeitor;
    FNFe: TNFe;
  public
    constructor Create(AOwner: TNFe);
    destructor Destroy; override;
    function LerXml: Boolean;
  published
    property Leitor: TLeitor read FLeitor write FLeitor;
    property NFe: TNFe       read FNFe    write FNFe;
  end;

implementation

uses
  pcnConversaoNFe,
  ACBrUtil.Base, ACBrUtil.Strings;

{ TNFeR }

constructor TNFeR.Create(AOwner: TNFe);
begin
  inherited Create;
  FLeitor := TLeitor.Create;
  FNFe    := AOwner;
end;

destructor TNFeR.Destroy;
begin
  FLeitor.Free;
  inherited Destroy;
end;

function TNFeR.LerXml: Boolean;
var
  ok: Boolean;
  i, j, k, nItem, idx: Integer;
  Arquivo, Itens, ItensTemp, VersaoInfNFe, NumItem: String;
  Aspas, tagPag, sAux: String;
begin

  Leitor.Grupo := Leitor.Arquivo;

  if Pos('versao="', Leitor.Arquivo) <> 0 then
    Aspas := '"'
   else
    Aspas := '''';

  NFe.infNFe.Id := Leitor.rAtributo('Id=', 'infNFe');
  if OnlyNumber(NFe.infNFe.Id) = '' then
    raise Exception.Create('Não encontrei o atributo: Id');

  VersaoInfNFe := Leitor.rAtributo('versao=', 'infNFe');
  if StringToFloatDef(VersaoInfNFe,-1) = -1 then
    raise Exception.Create('Não encontrei o atributo: versao');

  NFe.infNFe.Versao := StringToFloat(VersaoInfNFe);

  (* Grupo da TAG <ide> *******************************************************)
  if Leitor.rExtrai(1, 'ide') <> '' then
  begin
    (*B02*) NFe.ide.cUF := Leitor.rCampo(tcInt, 'cUF');
    (*B03*) NFe.ide.cNF := Leitor.rCampo(tcInt, 'cNF');
    if NFe.ide.cNF = 0 then
       NFe.ide.cNF := -2;
    (*B04*) NFe.ide.natOp  := Leitor.rCampo(tcStr, 'natOp');
    (*B05*) NFe.ide.indPag := StrToIndpag(ok, Leitor.rCampo(tcStr, 'indPag'));
    (*B06*) NFe.ide.modelo := Leitor.rCampo(tcInt, 'mod');
    (*B07*) NFe.ide.serie  := Leitor.rCampo(tcInt, 'serie');
    (*B08*) NFe.ide.nNF    := Leitor.rCampo(tcInt, 'nNF');

    if NFe.infNFe.Versao >= 3 then
     begin
      (*B09*) NFe.ide.dEmi    := Leitor.rCampo(tcDatHor, 'dhEmi');
      (*B10*) NFe.ide.dSaiEnt := Leitor.rCampo(tcDatHor, 'dhSaiEnt');
     end
    else
     begin
      (*B09*) NFe.ide.dEmi    := Leitor.rCampo(tcDat, 'dEmi');
      (*B10*) NFe.ide.dSaiEnt := Leitor.rCampo(tcDat, 'dSaiEnt');
      (*B10a*)NFe.ide.hSaiEnt := Leitor.rCampo(tcHor, 'hSaiEnt');
     end;
    (*B11*) NFe.ide.tpNF := StrToTpNF(ok, Leitor.rCampo(tcStr, 'tpNF'));

    if NFe.infNFe.Versao >= 3 then
     (*B11a*)NFe.ide.idDest := StrToDestinoOperacao(ok, Leitor.rCampo(tcStr, 'idDest'));

    (*B12*) NFe.ide.cMunFG := Leitor.rCampo(tcInt, 'cMunFG');

    (*B21*) NFe.Ide.tpImp  := StrToTpImp(ok, Leitor.rCampo(tcStr, 'tpImp'));
    (*B22*) NFe.Ide.tpEmis := StrToTpEmis(ok, Leitor.rCampo(tcStr, 'tpEmis'));
    (*B23*) NFe.Ide.cDV    := Leitor.rCampo(tcInt, 'cDV');
    (*B24*) NFe.Ide.tpAmb  := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
    (*B25*) NFe.Ide.finNFe := StrToFinNFe(ok, Leitor.rCampo(tcStr, 'finNFe'));

    if NFe.infNFe.Versao >= 3 then
     begin
      (*B25a*)NFe.ide.indFinal := StrToConsumidorFinal(ok, Leitor.rCampo(tcStr, 'indFinal'));
      (*B25b*)NFe.ide.indPres  := StrToPresencaComprador(ok, Leitor.rCampo(tcStr, 'indPres'));
     end;

    if NFe.infNFe.Versao >= 4 then
      NFe.ide.indIntermed := StrToIndIntermed(ok, Leitor.rCampo(tcStr, 'indIntermed'));

    (*B26*) NFe.Ide.procEmi := StrToProcEmi(ok, Leitor.rCampo(tcStr, 'procEmi'));
    (*B27*) NFe.Ide.verProc := Leitor.rCampo(tcStr, 'verProc');
    (*B28*) NFe.Ide.dhCont  := Leitor.rCampo(tcDatHor, 'dhCont');
    (*B29*) NFe.Ide.xJust   := Leitor.rCampo(tcStr, 'xJust');

    (* Grupo da TAG <ide><NFref> *)
    i := 0;
    Leitor.rExtrai(1, 'ide');
    NFe.Ide.NFref.Clear;
    while Leitor.rExtrai(2, 'NFref', '', i + 1) <> '' do
    begin
      NFe.Ide.NFref.New;
      (*B13*) NFe.ide.NFref[i].refNFe := Leitor.rCampo(tcEsp, 'refNFe');
              NFe.ide.NFref[i].refNFeSig := Leitor.rCampo(tcEsp, 'refNFeSig');

      if Length(Trim(Leitor.rCampo(tcEsp,'refNF'))) > 0 then // Verificação adicionada
      begin
        (*B15*) NFe.Ide.NFref[i].RefNF.cUF    := Leitor.rCampo(tcInt, 'cUF');
        (*B16*) NFe.Ide.NFref[i].RefNF.AAMM   := Leitor.rCampo(tcEsp, 'AAMM');
        (*B17*) NFe.Ide.NFref[i].RefNF.CNPJ   := Leitor.rCampo(tcEsp, 'CNPJ');
        (*B18*) NFe.Ide.NFref[i].RefNF.Modelo := StrToIntDef(Leitor.rCampo(tcInt, 'mod'),55);
        (*B19*) NFe.ide.NFref[i].RefNF.serie  := Leitor.rCampo(tcInt, 'serie');
        (*B20*) NFe.Ide.NFref[i].RefNF.nNF    := Leitor.rCampo(tcInt, 'nNF');
      end;

      //correção sugerida no MANTIS caso 814
      if Length(Trim(Leitor.rCampo(tcEsp,'refNFP'))) > 0 then
      begin
        (*B20b*) NFe.Ide.NFref[i].RefNFP.cUF     := Leitor.rCampo(tcInt, 'cUF');
        (*B20c*) NFe.Ide.NFref[i].RefNFP.AAMM    := Leitor.rCampo(tcEsp, 'AAMM');
   (*B20d/B20e*) NFe.Ide.NFref[i].RefNFP.CNPJCPF := Leitor.rCampoCNPJCPF;
        (*B20f*) NFe.Ide.NFref[i].RefNFP.IE      := Leitor.rCampo(tcEsp, 'IE');
        (*B20f*) NFe.Ide.NFref[i].RefNFP.Modelo  := Leitor.rCampo(tcInt, 'mod');
        (*B20g*) NFe.ide.NFref[i].RefNFP.serie   := Leitor.rCampo(tcInt, 'serie');
        (*B20h*) NFe.Ide.NFref[i].RefNFP.nNF     := Leitor.rCampo(tcInt, 'nNF');
      end;

      (*B20i*)NFe.ide.NFref[i].refCTe         := Leitor.rCampo(tcEsp, 'refCTe');

      (*B20k*) NFe.Ide.NFref[i].RefECF.modelo := StrToECFModRef(ok,Leitor.rCampo(tcStr, 'mod') ) ;
      (*B20l*) NFe.ide.NFref[i].RefECF.nECF   := Leitor.rCampo(tcStr, 'nECF');
      (*B20m*) NFe.Ide.NFref[i].RefECF.nCOO   := Leitor.rCampo(tcStr, 'nCOO');

      inc(i);
    end;

  end;

  (* Grupo da TAG <emit> ******************************************************)
  if Leitor.rExtrai(1, 'emit') <> '' then
  begin
    (*C02/C02a*)NFe.Emit.CNPJCPF := Leitor.rCampoCNPJCPF;
    (*C03*)NFe.Emit.xNome        := Leitor.rCampo(tcStr, 'xNome');
    (*C04*)NFe.Emit.xFant        := Leitor.rCampo(tcStr, 'xFant');
    (*C17*)NFe.Emit.IE           := Leitor.rCampo(tcStr, 'IE');
    (*C18*)NFe.Emit.IEST         := Leitor.rCampo(tcStr, 'IEST');
    (*C19*)NFe.Emit.IM           := Leitor.rCampo(tcStr, 'IM');
    (*C20*)NFe.Emit.CNAE         := Leitor.rCampo(tcStr, 'CNAE');
    (*C21*)NFe.Emit.CRT          := StrToCRT(ok, Leitor.rCampo(tcStr, 'CRT'));
  end;

  (* Grupo da TAG <emit><EnderEmit> *)
  if Leitor.rExtrai(1, 'emit') <> '' then
  begin
    if Leitor.rExtrai(2, 'enderEmit') <> '' then
    begin
      (*C06*)NFe.Emit.enderEmit.xLgr    := Leitor.rCampo(tcStr, 'xLgr');
      (*C07*)NFe.Emit.enderEmit.nro     := Leitor.rCampo(tcStr, 'nro');
      (*C08*)NFe.Emit.enderEmit.xCpl    := Leitor.rCampo(tcStr, 'xCpl');
      (*C09*)NFe.Emit.enderEmit.xBairro := Leitor.rCampo(tcStr, 'xBairro');
      (*C10*)NFe.Emit.EnderEmit.cMun    := Leitor.rCampo(tcInt, 'cMun');
      (*C11*)NFe.Emit.enderEmit.xMun    := Leitor.rCampo(tcStr, 'xMun');
      (*C12*)NFe.Emit.enderEmit.UF      := Leitor.rCampo(tcStr, 'UF');
      (*C13*)NFe.Emit.enderEmit.CEP     := Leitor.rCampo(tcInt, 'CEP');
      (*C14*)NFe.Emit.enderEmit.cPais   := Leitor.rCampo(tcInt, 'cPais');

      if NFe.Emit.enderEmit.cPais = 0 then
        NFe.Emit.enderEmit.cPais := 1058;

      (*C15*)NFe.Emit.enderEmit.xPais   := Leitor.rCampo(tcStr, 'xPais');

      if NFe.Emit.enderEmit.xPais = '' then
        NFe.Emit.enderEmit.xPais := 'BRASIL';
        
      (*C16*)NFe.Emit.enderEmit.fone    := Leitor.rCampo(tcStr, 'fone');
    end;
  end;

  (* Grupo da TAG <avulsa> ****************************************************)
  if Leitor.rExtrai(1, 'avulsa') <> '' then
  begin
    (*D02*)NFe.Avulsa.CNPJ    := Leitor.rCampo(tcStr, 'CNPJ');
    (*D03*)NFe.Avulsa.xOrgao  := Leitor.rCampo(tcStr, 'xOrgao');
    (*D04*)NFe.Avulsa.matr    := Leitor.rCampo(tcStr, 'matr');
    (*D05*)NFe.Avulsa.xAgente := Leitor.rCampo(tcStr, 'xAgente');
    (*D06*)NFe.Avulsa.fone    := Leitor.rCampo(tcStr, 'fone');
    (*D07*)NFe.Avulsa.UF      := Leitor.rCampo(tcStr, 'UF');
    (*D08*)NFe.Avulsa.nDAR    := Leitor.rCampo(tcStr, 'nDAR');
    (*D09*)NFe.Avulsa.dEmi    := Leitor.rCampo(tcDat, 'dEmi');
    (*D10*)NFe.Avulsa.vDAR    := Leitor.rCampo(tcDe2, 'vDAR');
    (*D11*)NFe.Avulsa.repEmi  := Leitor.rCampo(tcStr, 'repEmi');
    (*D12*)NFe.Avulsa.dPag    := Leitor.rCampo(tcDat, 'dPag');
  end;

  (* Grupo da TAG <dest> ******************************************************)
  if Leitor.rExtrai(1, 'dest') <> '' then
  begin
    (*E02/E03*)NFe.Dest.CNPJCPF := Leitor.rCampoCNPJCPF;

    if NFe.infNFe.Versao >= 3 then
     (*E03a*)NFe.Dest.idEstrangeiro := Leitor.rCampo(tcStr, 'idEstrangeiro');

    (*E04*)NFe.Dest.xNome := Leitor.rCampo(tcStr, 'xNome');

    if NFe.infNFe.Versao >= 3 then
     (*E16a*)NFe.Dest.indIEDest := StrToindIEDest(Ok, Leitor.rCampo(tcStr, 'indIEDest'));

    (*E17*)NFe.Dest.IE := Leitor.rCampo(tcStr, 'IE');
    (*E18*)NFe.Dest.ISUF := Leitor.rCampo(tcStr, 'ISUF');

    if NFe.infNFe.Versao >= 3 then
     (*E18a*)NFe.Dest.IM := Leitor.rCampo(tcStr, 'IM');

    (*E19*)NFe.Dest.Email := Leitor.rCampo(tcStr, 'email');
  end
  else
    NFe.Dest.indIEDest := inNaoContribuinte;

  (* Grupo da TAG <dest> <EnderDest> *)
  if Leitor.rExtrai(1, 'dest') <> '' then
  begin
    if Leitor.rExtrai(2, 'enderDest') <> '' then
    begin
      (*E06*)NFe.Dest.enderDest.xLgr    := Leitor.rCampo(tcStr, 'xLgr');
      (*E07*)NFe.Dest.enderDest.nro     := Leitor.rCampo(tcStr, 'nro');
      (*E08*)NFe.Dest.enderDest.xCpl    := Leitor.rCampo(tcStr, 'xCpl');
      (*E09*)NFe.Dest.enderDest.xBairro := Leitor.rCampo(tcStr, 'xBairro');
      (*E10*)NFe.Dest.enderDest.cMun    := Leitor.rCampo(tcInt, 'cMun');
      (*E11*)NFe.Dest.enderDest.xMun    := Leitor.rCampo(tcStr, 'xMun');
      (*E12*)NFe.Dest.enderDest.UF      := Leitor.rCampo(tcStr, 'UF');
      (*E13*)NFe.Dest.enderDest.CEP     := Leitor.rCampo(tcInt, 'CEP');
      (*E14*)NFe.Dest.enderDest.cPais   := Leitor.rCampo(tcInt, 'cPais');

      if NFe.Dest.enderDest.cPais = 0 then
        NFe.Dest.enderDest.cPais := 1058;

      (*E15*)NFe.Dest.enderDest.xPais   := Leitor.rCampo(tcStr, 'xPais');

      if NFe.Dest.enderDest.xPais = '' then
        NFe.Dest.enderDest.xPais := 'BRASIL';

      (*E16*)NFe.Dest.enderDest.fone    := Leitor.rCampo(tcStr, 'fone');
    end;
  end;

  (* Grupo da TAG <retirada> **************************************************)
  if Leitor.rExtrai(1, 'retirada') <> '' then
  begin
    (*F02/F02a*)NFe.Retirada.CNPJCPF := Leitor.rCampoCNPJCPF;
    (*F02b*)NFe.Retirada.xNome       := Leitor.rCampo(tcStr, 'xNome');
    (*F03*)NFe.Retirada.xLgr         := Leitor.rCampo(tcStr, 'xLgr');
    (*F04*)NFe.Retirada.nro          := Leitor.rCampo(tcStr, 'nro');
    (*F05*)NFe.Retirada.xCpl         := Leitor.rCampo(tcStr, 'xCpl');
    (*F06*)NFe.Retirada.xBairro      := Leitor.rCampo(tcStr, 'xBairro');
    (*F07*)NFe.Retirada.cMun         := Leitor.rCampo(tcInt, 'cMun');
    (*F08*)NFe.Retirada.xMun         := Leitor.rCampo(tcStr, 'xMun');
    (*F09*)NFe.Retirada.UF           := Leitor.rCampo(tcStr, 'UF');
    (*F10*)NFe.Retirada.CEP          := Leitor.rCampo(tcInt, 'CEP');
    (*F11*)NFe.Retirada.cPais        := Leitor.rCampo(tcInt, 'cPais');
    (*F12*)NFe.Retirada.xPais        := Leitor.rCampo(tcStr, 'xPais');
    (*F13*)NFe.Retirada.fone         := Leitor.rCampo(tcStr, 'fone');
    (*F14*)NFe.Retirada.Email        := Leitor.rCampo(tcStr, 'email');
    (*F15*)NFe.Retirada.IE           := Leitor.rCampo(tcStr, 'IE');
  end;

  (* Grupo da TAG <entrega> ***************************************************)
  if Leitor.rExtrai(1, 'entrega') <> '' then
  begin
    (*G02/G02a*)NFe.Entrega.CNPJCPF := Leitor.rCampoCNPJCPF;
    (*G02b*)NFe.Entrega.xNome       := Leitor.rCampo(tcStr, 'xNome');
    (*G03*)NFe.Entrega.xLgr         := Leitor.rCampo(tcStr, 'xLgr');
    (*G04*)NFe.Entrega.nro          := Leitor.rCampo(tcStr, 'nro');
    (*G05*)NFe.Entrega.xCpl         := Leitor.rCampo(tcStr, 'xCpl');
    (*G06*)NFe.Entrega.xBairro      := Leitor.rCampo(tcStr, 'xBairro');
    (*G07*)NFe.Entrega.cMun         := Leitor.rCampo(tcInt, 'cMun');
    (*G08*)NFe.Entrega.xMun         := Leitor.rCampo(tcStr, 'xMun');
    (*G09*)NFe.Entrega.UF           := Leitor.rCampo(tcStr, 'UF');
    (*G10*)NFe.Entrega.CEP          := Leitor.rCampo(tcInt, 'CEP');
    (*G11*)NFe.Entrega.cPais        := Leitor.rCampo(tcInt, 'cPais');
    (*G12*)NFe.Entrega.xPais        := Leitor.rCampo(tcStr, 'xPais');
    (*G13*)NFe.Entrega.fone         := Leitor.rCampo(tcStr, 'fone');
    (*G14*)NFe.Entrega.Email        := Leitor.rCampo(tcStr, 'email');
    (*G15*)NFe.Entrega.IE           := Leitor.rCampo(tcStr, 'IE');
  end;

  (* Grupo da TAG <autXML> ****************************************************)
  i := 0;
  NFe.autXML.Clear;
  while Leitor.rExtrai(1, 'autXML', '', i + 1) <> '' do
  begin
    NFe.autXML.New;
    NFe.autXML[i].CNPJCPF := Leitor.rCampoCNPJCPF;;
    inc(i);
  end;

  (* Grupo da TAG <det> *******************************************************)
  i := 0;
  Arquivo := Leitor.Arquivo;

  Itens := copy(
    Arquivo,
    Pos('<det nItem=', Arquivo),
    Pos('<total', Arquivo) - Pos('<det nItem=',Arquivo));

  Itens := StringReplace(Itens, #$D#$A, '', [rfReplaceAll]);

  ItensTemp := copy(
    Itens,
    Pos('<det nItem=', Itens),
    (Pos('</det>', Itens) + 6) - Pos('<det nItem=', Itens)
  );
  NFe.Det.Clear;
  while pos('<det nItem=',ItensTemp) <> 0 do
  begin
    Leitor.Arquivo := 'Item '+ItensTemp;

    NumItem   := copy(ItensTemp,Pos('nItem=',ItensTemp)+7,Pos(Aspas,ItensTemp));
    NumItem   := copy(NumItem,1,Pos(Aspas,NumItem)-1);
    nItem     := StrToInt(NumItem);
    Itens     := StringReplace(Itens, ItensTemp, '',[]);
    ItensTemp := copy(Itens,Pos('<det nItem=',Itens),(Pos('</det>',Itens)+6)-Pos('<det nItem=',Itens));

    Leitor.rExtrai(1, 'det nItem=' + Aspas + IntToStr(nItem) + Aspas, 'det');
    NFe.Det.New;
    (*   *)NFe.Det[i].prod.nItem := nItem;
    (*V01*)NFe.Det[i].infAdProd  := Leitor.rCampo(tcStr, 'infAdProd');

    (* Grupo da TAG <det><prod> *)
    Leitor.rExtrai(2, 'prod');
    (*I02*)NFe.Det[i].Prod.cProd := Leitor.rCampo(tcStr, 'cProd');
    (*I03*)NFe.Det[i].Prod.cEAN  := Leitor.rCampo(tcStr, 'cEAN');
           NFe.Det[i].Prod.cBarra  := Leitor.rCampo(tcStr, 'cBarra');
    (*I04*)NFe.Det[i].Prod.xProd := Leitor.rCampo(tcStr, 'xProd');
    (*I05*)NFe.Det[i].Prod.NCM   := Leitor.rCampo(tcStr, 'NCM');
    (*I05w*)NFe.Det[i].Prod.CEST := Leitor.rCampo(tcStr, 'CEST');
    if NFe.infNFe.Versao >= 4 then
    begin
      (*I05d*)NFe.Det[i].Prod.indEscala := StrToindEscala(ok, Leitor.rCampo(tcStr, 'indEscala'));
      (*I05e*)NFe.Det[i].Prod.CNPJFab   := Leitor.rCampo(tcStr, 'CNPJFab');
      (*I05f*)NFe.Det[i].Prod.cBenef    := Leitor.rCampo(tcStr, 'cBenef');

      idx := 0;
      NFe.Det[i].Prod.CredPresumido.Clear;
      while Leitor.rExtrai(2, 'cCredPresumido', '', idx + 1) <> '' do
      begin
        NFe.Det[i].Prod.CredPresumido.New;
        NFe.Det[i].Prod.CredPresumido[idx].cCredPresumido := Leitor.rCampo(tcStr, 'cCredPresumido');

        Leitor.rExtrai(2, 'pCredPresumido', '', idx + 1);
        NFe.Det[i].Prod.CredPresumido[idx].pCredPresumido := Leitor.rCampo(tcDe2, 'pCredPresumido');

        Leitor.rExtrai(2, 'vCredPresumido', '', idx + 1);
        NFe.Det[i].Prod.CredPresumido[idx].vCredPresumido := Leitor.rCampo(tcDe2, 'vCredPresumido');

        inc(idx);
      end;
    end;

    Leitor.rExtrai(2, 'prod');
    (*I06*)NFe.Det[i].Prod.EXTIPI   := Leitor.rCampo(tcStr, 'EXTIPI');
    //(*I07*)NFe.Det[i].Prod.genero := Leitor.rCampo(tcInt, 'genero');
    (*I08*)NFe.Det[i].Prod.CFOP     := Leitor.rCampo(tcEsp, 'CFOP');
    (*I09*)NFe.Det[i].Prod.uCom     := Leitor.rCampo(tcStr, 'uCom');
    (*I10*)NFe.Det[i].Prod.qCom     := Leitor.rCampo(tcDe4, 'qCom');
    (*I10a*)NFe.Det[i].Prod.vUnCom  := Leitor.rCampo(tcDe10, 'vUnCom');
    (*I11*)NFe.Det[i].Prod.vProd    := Leitor.rCampo(tcDe2, 'vProd');
    (*I12*)NFe.Det[i].Prod.cEANTrib := Leitor.rCampo(tcStr, 'cEANTrib');
           NFe.Det[i].Prod.cBarraTrib := Leitor.rCampo(tcStr, 'cBarraTrib');
    (*I13*)NFe.Det[i].Prod.uTrib    := Leitor.rCampo(tcStr, 'uTrib');
    (*I14*)NFe.Det[i].Prod.qTrib    := Leitor.rCampo(tcDe4, 'qTrib');
    (*I14a*)NFe.Det[i].Prod.vUnTrib := Leitor.rCampo(tcDe10, 'vUnTrib');
    (*I15*)NFe.Det[i].Prod.vFrete   := Leitor.rCampo(tcDe2, 'vFrete');
    (*I16*)NFe.Det[i].Prod.vSeg     := Leitor.rCampo(tcDe2, 'vSeg');
    (*I17*)NFe.Det[i].Prod.vDesc    := Leitor.rCampo(tcDe2, 'vDesc');
    (*I17a*)NFe.Det[i].Prod.vOutro  := Leitor.rCampo(tcDe2, 'vOutro');
    (*I17b*)NFe.Det[i].Prod.IndTot  := StrToindTot(ok,Leitor.rCampo(tcDe2, 'indTot'));
    (*I30*)NFe.Det[i].Prod.xPed     := Leitor.rCampo(tcStr, 'xPed');
    (*I31*)NFe.Det[i].Prod.nItemPed := Leitor.rCampo(tcStr, 'nItemPed');
    (*I31*)NFe.Det[i].Prod.nRECOPI  := Leitor.rCampo(tcStr, 'nRECOPI');
    (*I70*)NFe.Det[i].Prod.nFCI     := Leitor.rCampo(tcStr, 'nFCI');

    j := 0;
    NFe.Det[i].Prod.NVE.Clear;
    while Leitor.rExtrai(3, 'NVE', '', j + 1) <> '' do
    begin
      NFe.Det[i].Prod.NVE.New;
      (*I05a*) NFe.Det[i].Prod.NVE[j].NVE := Leitor.rCampo(tcStr, 'NVE');

      inc(j);
    end;
    

    (* Grupo da TAG <det><prod><DI> *)
    j := 0;
    NFe.Det[i].Prod.DI.Clear;
    while Leitor.rExtrai(3, 'DI', '', j + 1) <> '' do
    begin
      NFe.Det[i].Prod.DI.New;
      (*I19*)NFe.Det[i].Prod.DI[j].nDI        := Leitor.rCampo(tcStr, 'nDI');
      (*I20*)NFe.Det[i].Prod.DI[j].dDI        := Leitor.rCampo(tcDat, 'dDI');
      (*I21*)NFe.Det[i].Prod.DI[j].xLocDesemb := Leitor.rCampo(tcStr, 'xLocDesemb');
      (*I22*)NFe.Det[i].Prod.DI[j].UFDesemb   := Leitor.rCampo(tcStr, 'UFDesemb');
      (*I23*)NFe.Det[i].Prod.DI[j].dDesemb    := Leitor.rCampo(tcDat, 'dDesemb');

      (*I23a*)NFe.Det[i].Prod.DI[j].tpViaTransp  := StrToTipoViaTransp(Ok, Leitor.rCampo(tcInt, 'tpViaTransp'));
      (*I23b*)NFe.Det[i].Prod.DI[j].vAFRMM       := Leitor.rCampo(tcDe2, 'vAFRMM');
      (*I23c*)NFe.Det[i].Prod.DI[j].tpIntermedio := StrToTipoIntermedio(Ok, Leitor.rCampo(tcInt, 'tpIntermedio'));
      (*I23d*)NFe.Det[i].Prod.DI[j].CNPJ         := Leitor.rCampoCNPJCPF;;
      (*I23e*)NFe.Det[i].Prod.DI[j].UFTerceiro   := Leitor.rCampo(tcStr, 'UFTerceiro');

      (*I24*)NFe.Det[i].Prod.DI[j].cExportador   := Leitor.rCampo(tcStr, 'cExportador');

      (* Grupo da TAG <det><prod><DI><adi> *)
      k := 0;
      NFe.Det[i].Prod.DI[j].adi.Clear;
      while Leitor.rExtrai(4, 'adi', '', k + 1) <> '' do
      begin
        NFe.Det[i].Prod.DI[j].adi.New;
        (*I26*)NFe.Det[i].Prod.DI[j].adi[k].nAdicao     := Leitor.rCampo(tcInt, 'nAdicao');
        (*I27*)NFe.Det[i].Prod.DI[j].adi[k].nSeqAdi     := Leitor.rCampo(tcInt, 'nSeqAdic');
        (*I28*)NFe.Det[i].Prod.DI[j].adi[k].cFabricante := Leitor.rCampo(tcStr, 'cFabricante');
        (*I29*)NFe.Det[i].Prod.DI[j].adi[k].vDescDI     := Leitor.rCampo(tcDe2, 'vDescDI');

        (*I29a*)NFe.Det[i].Prod.DI[j].adi[k].nDraw      := Leitor.rCampo(tcStr, 'nDraw');
        inc(k);
      end;

      inc(j);
    end;

    (* Grupo da TAG <det><prod><detExport> *)
    j := 0;
    NFe.Det[i].Prod.detExport.Clear;
    while Leitor.rExtrai(3, 'detExport', '', j + 1) <> '' do
    begin
      NFe.Det[i].Prod.detExport.New;
      (*I51*)NFe.Det[i].Prod.detExport[j].nDraw := Leitor.rCampo(tcStr, 'nDraw');

      Leitor.rExtrai(4, 'exportInd');
      (*I53*)NFe.Det[i].Prod.detExport[j].nRE     := Leitor.rCampo(tcStr, 'nRE');
      (*I54*)NFe.Det[i].Prod.detExport[j].chNFe   := Leitor.rCampo(tcStr, 'chNFe');
      (*I55*)NFe.Det[i].Prod.detExport[j].qExport := Leitor.rCampo(tcDe4, 'qExport');

      inc(j);
    end;

    (* Grupo da TAG <det><prod><rastro> *)
    j := 0;
    NFe.Det[i].Prod.rastro.Clear;
    while Leitor.rExtrai(3, 'rastro', '', j + 1) <> '' do
    begin
      NFe.Det[i].Prod.rastro.New;
      (*I81*)NFe.Det[i].Prod.rastro[j].nLote  := Leitor.rCampo(tcStr, 'nLote');
      (*I82*)NFe.Det[i].Prod.rastro[j].qLote  := Leitor.rCampo(tcDe3, 'qLote');
      (*I83*)NFe.Det[i].Prod.rastro[j].dFab   := Leitor.rCampo(tcDat, 'dFab ');
      (*I84*)NFe.Det[i].Prod.rastro[j].dVal   := Leitor.rCampo(tcDat, 'dVal ');
      (*I85*)NFe.Det[i].Prod.rastro[j].cAgreg := Leitor.rCampo(tcStr, 'cAgreg');
      inc(j);
    end;

    (* Grupo da TAG <det><prod><veicProd> *)
    if Leitor.rExtrai(3, 'veicProd') <> '' then
    begin
      (*J02*)NFe.Det[i].Prod.veicProd.tpOP         := StrToTpOP(ok, Leitor.rCampo(tcStr, 'tpOp'));
      (*J03*)NFe.Det[i].Prod.veicProd.chassi       := Leitor.rCampo(tcStr, 'chassi');
      (*J04*)NFe.Det[i].Prod.veicProd.cCor         := Leitor.rCampo(tcStr, 'cCor');
      (*J05*)NFe.Det[i].Prod.veicProd.xCor         := Leitor.rCampo(tcStr, 'xCor');
      (*J06*)NFe.Det[i].Prod.veicProd.pot          := Leitor.rCampo(tcStr, 'pot');
      (*J07*)NFe.Det[i].Prod.veicProd.Cilin        := Leitor.rCampo(tcStr, 'cilin');
      (*J08*)NFe.Det[i].Prod.veicProd.pesoL        := Leitor.rCampo(tcStr, 'pesoL');
      (*J09*)NFe.Det[i].Prod.veicProd.pesoB        := Leitor.rCampo(tcStr, 'pesoB');
      (*J10*)NFe.Det[i].Prod.veicProd.nSerie       := Leitor.rCampo(tcStr, 'nSerie');
      (*J11*)NFe.Det[i].Prod.veicProd.tpComb       := Leitor.rCampo(tcStr, 'tpComb');
      (*J12*)NFe.Det[i].Prod.veicProd.nMotor       := Leitor.rCampo(tcStr, 'nMotor');
      (*J13*)NFe.Det[i].Prod.veicProd.CMT          := Leitor.rCampo(tcStr, 'CMT');
      (*J14*)NFe.Det[i].Prod.veicProd.dist         := Leitor.rCampo(tcStr, 'dist');
      //(*J15*)NFe.Det[i].Prod.veicProd.RENAVAM := Leitor.rCampo(tcEsp, 'RENAVAM');
      (*J16*)NFe.Det[i].Prod.veicProd.anoMod       := Leitor.rCampo(tcInt, 'anoMod');
      (*J17*)NFe.Det[i].Prod.veicProd.anoFab       := Leitor.rCampo(tcInt, 'anoFab');
      (*J18*)NFe.Det[i].Prod.veicProd.tpPint       := Leitor.rCampo(tcStr, 'tpPint');
      (*J19*)NFe.Det[i].Prod.veicProd.tpVeic       := Leitor.rCampo(tcInt, 'tpVeic');
      (*J20*)NFe.Det[i].Prod.veicProd.espVeic      := Leitor.rCampo(tcInt, 'espVeic');
      (*J21*)NFe.Det[i].Prod.veicProd.VIN          := Leitor.rCampo(tcStr, 'VIN');
      (*J22*)NFe.Det[i].Prod.veicProd.condVeic     := StrToCondVeic(ok, Leitor.rCampo(tcStr, 'condVeic'));
      (*J23*)NFe.Det[i].Prod.veicProd.cMod         := Leitor.rCampo(tcStr, 'cMod');
      (*J24*)NFe.Det[i].Prod.veicProd.cCorDENATRAN := Leitor.rCampo(tcStr, 'cCorDENATRAN');
      (*J25*)NFe.Det[i].Prod.veicProd.lota         := Leitor.rCampo(tcInt, 'lota');
      (*J26*)NFe.Det[i].Prod.veicProd.tpRest       := Leitor.rCampo(tcInt, 'tpRest');
    end;

    (* Grupo da TAG <det><prod><med> *)
    j := 0;
    NFe.Det[i].Prod.med.Clear;
    while Leitor.rExtrai(3, 'med', '', j + 1) <> '' do
    begin
      NFe.Det[i].Prod.med.New;
      (*K01a*)NFe.Det[i].Prod.med[j].cProdANVISA := Leitor.rCampo(tcStr, 'cProdANVISA');
      (*K01b*)NFe.Det[i].Prod.med[j].xMotivoIsencao := Leitor.rCampo(tcStr, 'xMotivoIsencao');
      (*K02*)NFe.Det[i].Prod.med[j].nLote := Leitor.rCampo(tcStr, 'nLote');
      (*K03*)NFe.Det[i].Prod.med[j].qLote := Leitor.rCampo(tcDe3, 'qLote');
      (*K04*)NFe.Det[i].Prod.med[j].dFab  := Leitor.rCampo(tcDat, 'dFab ');
      (*K05*)NFe.Det[i].Prod.med[j].dVal  := Leitor.rCampo(tcDat, 'dVal ');
      (*K06*)NFe.Det[i].Prod.med[j].vPMC  := Leitor.rCampo(tcDe2, 'vPMC ');
      inc(j);
    end;

    (* Grupo da TAG <det><prod><arma> *)
    j := 0;
    NFe.Det[i].Prod.arma.Clear;
    while Leitor.rExtrai(3, 'arma', '', j + 1) <> '' do
    begin
      NFe.Det[i].Prod.arma.New;
      (*L02*)NFe.Det[i].Prod.arma[j].tpArma := StrToTpArma(ok, Leitor.rCampo(tcStr, 'tpArma'));
      (*L03*)NFe.Det[i].Prod.arma[j].nSerie := Leitor.rCampo(tcStr, 'nSerie');
      (*L04*)NFe.Det[i].Prod.arma[j].nCano  := Leitor.rCampo(tcStr, 'nCano');
      (*L05*)NFe.Det[i].Prod.arma[j].descr  := Leitor.rCampo(tcStr, 'descr');
      inc(j);
    end;

    (* Grupo da TAG <det><prod><comb> *)
    if Leitor.rExtrai(3, 'comb') <> '' then
    begin
      (*L102*)NFe.Det[i].Prod.comb.cProdANP := Leitor.rCampo(tcInt, 'cProdANP');
      (*L102a*)NFe.Det[i].Prod.comb.pMixGN  := Leitor.rCampo(tcDe4, 'qMixGN');
      (*LA03*)NFe.Det[i].Prod.comb.descANP  := Leitor.rCampo(tcStr, 'descANP');
      (*LA03a*)NFe.Det[i].Prod.comb.pGLP    := Leitor.rCampo(tcDe4, 'pGLP');
      (*LA03b*)NFe.Det[i].Prod.comb.pGNn    := Leitor.rCampo(tcDe4, 'pGNn');
      (*LA03c*)NFe.Det[i].Prod.comb.pGNi    := Leitor.rCampo(tcDe4, 'pGNi');
      (*LA03d*)NFe.Det[i].Prod.comb.vPart   := Leitor.rCampo(tcDe2, 'vPart');
      (*LA04*)NFe.Det[i].Prod.comb.CODIF    := Leitor.rCampo(tcEsp, 'CODIF');
      (*LA05*)NFe.Det[i].Prod.comb.qTemp    := Leitor.rCampo(tcDe4, 'qTemp');
      (*LA06*)NFe.Det[i].Prod.comb.UFcons   := Leitor.rCampo(tcStr, 'UFCons');

      (*L120*)NFe.Det[i].Prod.comb.ICMSCons.UFcons := Leitor.rCampo(tcStr, 'UFcons');

      NFe.Det[i].Prod.comb.pBio := Leitor.rCampo(tcDe4, 'pBio');

      j := 0;
      NFe.Det[i].Prod.comb.origComb.Clear;
      while Leitor.rExtrai(4, 'origComb', '', j + 1) <> '' do
      begin
        NFe.Det[i].Prod.comb.origComb.New;
        NFe.Det[i].Prod.comb.origComb[j].indImport := StrToindImport(ok, Leitor.rCampo(tcStr, 'indImport'));
        NFe.Det[i].Prod.comb.origComb[j].cUFOrig := Leitor.rCampo(tcInt, 'cUFOrig');
        NFe.Det[i].Prod.comb.origComb[j].pOrig := Leitor.rCampo(tcDe4, 'pOrig');

        inc(j);
      end;

      if Leitor.rExtrai(4, 'CIDE') <> '' then
      begin
        (*L106*)NFe.Det[i].Prod.comb.CIDE.qBCprod   := Leitor.rCampo(tcDe4, 'qBCProd');
        (*L107*)NFe.Det[i].Prod.comb.CIDE.vAliqProd := Leitor.rCampo(tcDe4, 'vAliqProd');
        (*L108*)NFe.Det[i].Prod.comb.CIDE.vCIDE     := Leitor.rCampo(tcDe2, 'vCIDE');
      end;

      if Leitor.rExtrai(4, 'encerrante') <> '' then
      begin
        (*LA12*)NFe.Det[i].Prod.comb.encerrante.nBico   := Leitor.rCampo(tcInt, 'nBico');
        (*LA13*)NFe.Det[i].Prod.comb.encerrante.nBomba  := Leitor.rCampo(tcInt, 'nBomba');
        (*LA14*)NFe.Det[i].Prod.comb.encerrante.nTanque := Leitor.rCampo(tcInt, 'nTanque');
        (*LA15*)NFe.Det[i].Prod.comb.encerrante.vEncIni := Leitor.rCampo(tcDe3, 'vEncIni');
        (*LA16*)NFe.Det[i].Prod.comb.encerrante.vEncFin := Leitor.rCampo(tcDe3, 'vEncFin');
      end;

      if Leitor.rExtrai(4, 'ICMSComb') <> '' then
      begin
        (*L110*)NFe.Det[i].Prod.comb.ICMS.vBCICMS   := Leitor.rCampo(tcDe2, 'vBCICMS');
        (*L111*)NFe.Det[i].Prod.comb.ICMS.vICMS     := Leitor.rCampo(tcDe2, 'vICMS');
        (*L112*)NFe.Det[i].Prod.comb.ICMS.vBCICMSST := Leitor.rCampo(tcDe2, 'vBCICMSST');
        (*L113*)NFe.Det[i].Prod.comb.ICMS.vICMSST   := Leitor.rCampo(tcDe2, 'vICMSST');
      end;
      if Leitor.rExtrai(4, 'ICMSInter') <> '' then
      begin
        (*L115*)NFe.Det[i].Prod.comb.ICMSInter.vBCICMSSTDest := Leitor.rCampo(tcDe2, 'vBCICMSSTDest');
        (*L116*)NFe.Det[i].Prod.comb.ICMSInter.vICMSSTDest   := Leitor.rCampo(tcDe2, 'vICMSSTDest');
      end;
      if Leitor.rExtrai(4, 'ICMSCons') <> '' then
      begin
        (*L118*)NFe.Det[i].Prod.comb.ICMSCons.vBCICMSSTCons := Leitor.rCampo(tcDe2, 'vBCICMSSTCons');
        (*L119*)NFe.Det[i].Prod.comb.ICMSCons.vICMSSTCons   := Leitor.rCampo(tcDe2, 'vICMSSTCons');
        (*L119*)NFe.Det[i].Prod.comb.ICMSCons.UFcons        := Leitor.rCampo(tcStr, 'UFCons');
      end;
    end;

    (* Grupo da TAG <det><imposto> ********************************************)
    Leitor.rExtrai(2, 'imposto');
    (*M02*)NFe.Det[i].Imposto.vTotTrib := Leitor.rCampo(tcDe2, 'vTotTrib');

    if Leitor.rExtrai(3, 'ICMS') <> '' then
    begin
      (*N11*)NFe.Det[i].Imposto.ICMS.orig        := StrToOrig(ok, Leitor.rCampo(tcStr, 'orig'));
      (*N12*)NFe.Det[i].Imposto.ICMS.CST         := StrToCSTICMS(ok, Leitor.rCampo(tcStr, 'CST'));
      (*N12a*)NFe.Det[i].Imposto.ICMS.CSOSN      := StrToCSOSNIcms( ok,Leitor.rCampo(tcInt, 'CSOSN'));
      (*N13*)NFe.Det[i].Imposto.ICMS.modBC       := StrToModBC(ok, Leitor.rCampo(tcStr, 'modBC'));
      (*N14*)NFe.Det[i].Imposto.ICMS.pRedBC      := Leitor.rCampo(tcDe2, 'pRedBC');
      (*N15*)NFe.Det[i].Imposto.ICMS.vBC         := Leitor.rCampo(tcDe2, 'vBC');
      (*N16*)NFe.Det[i].Imposto.ICMS.pICMS       := Leitor.rCampo(tcDe2, 'pICMS');
      (*N16a*)NFe.Det[i].Imposto.ICMS.vICMSOp    := Leitor.rCampo(tcDe2, 'vICMSOp');
      (*N16b*)NFe.Det[i].Imposto.ICMS.pDif       := Leitor.rCampo(tcDe4, 'pDif');
      (*N16c*)NFe.Det[i].Imposto.ICMS.vICMSDif   := Leitor.rCampo(tcDe2, 'vICMSDif');
      (*N17*)NFe.Det[i].Imposto.ICMS.vICMS       := Leitor.rCampo(tcDe2, 'vICMS');
      (*N17a*)NFe.Det[i].Imposto.ICMS.vBCFCP     := Leitor.rCampo(tcDe2, 'vBCFCP');
      (*N17b*)NFe.Det[i].Imposto.ICMS.pFCP       := Leitor.rCampo(tcDe4, 'pFCP');
      (*N17c*)NFe.Det[i].Imposto.ICMS.vFCP       := Leitor.rCampo(tcDe2, 'vFCP');
      (*N18*)NFe.Det[i].Imposto.ICMS.modBCST     := StrToModBCST(ok, Leitor.rCampo(tcStr, 'modBCST'));
      (*N19*)NFe.Det[i].Imposto.ICMS.pMVAST      := Leitor.rCampo(tcDe2, 'pMVAST');
      (*N20*)NFe.Det[i].Imposto.ICMS.pRedBCST    := Leitor.rCampo(tcDe2, 'pRedBCST');
      (*N21*)NFe.Det[i].Imposto.ICMS.vBCST       := Leitor.rCampo(tcDe2, 'vBCST');
      (*N22*)NFe.Det[i].Imposto.ICMS.pICMSST     := Leitor.rCampo(tcDe2, 'pICMSST');
      (*N23*)NFe.Det[i].Imposto.ICMS.vICMSST     := Leitor.rCampo(tcDe2, 'vICMSST');
      (*N23a*)NFe.Det[i].Imposto.ICMS.vBCFCPST   := Leitor.rCampo(tcDe2, 'vBCFCPST');
      (*N23b*)NFe.Det[i].Imposto.ICMS.pFCPST     := Leitor.rCampo(tcDe4, 'pFCPST');
      (*N23d*)NFe.Det[i].Imposto.ICMS.vFCPST     := Leitor.rCampo(tcDe2, 'vFCPST');
      (*N24*)NFe.Det[i].Imposto.ICMS.UFST        := Leitor.rCampo(tcStr, 'UFST');
      (*N25*)NFe.Det[i].Imposto.ICMS.pBCOp       := Leitor.rCampo(tcDe2, 'pBCOp');
      (*N26*)NFe.Det[i].Imposto.ICMS.vBCSTRet    := Leitor.rCampo(tcDe2, 'vBCSTRet');
      (*N27*)NFe.Det[i].Imposto.ICMS.vICMSSTRet  := Leitor.rCampo(tcDe2, 'vICMSSTRet');
      (*N27a*)NFe.Det[i].Imposto.ICMS.vICMSDeson := Leitor.rCampo(tcDe2, 'vICMSDeson');
      (*N27a*)NFe.Det[i].Imposto.ICMS.vBCFCPSTRet:= Leitor.rCampo(tcDe2, 'vBCFCPSTRet');
      (*N27b*)NFe.Det[i].Imposto.ICMS.pFCPSTRet  := Leitor.rCampo(tcDe4, 'pFCPSTRet');
      (*N27d*)NFe.Det[i].Imposto.ICMS.vFCPSTRet  := Leitor.rCampo(tcDe2, 'vFCPSTRet');
      (*N27e*)NFe.Det[i].Imposto.ICMS.pST        := Leitor.rCampo(tcDe4, 'pST');
      (*N28*)NFe.Det[i].Imposto.ICMS.motDesICMS  := StrTomotDesICMS(ok, Leitor.rCampo(tcStr, 'motDesICMS'));
      (*N29*)NFe.Det[i].Imposto.ICMS.pCredSN     := Leitor.rCampo(tcDe2, 'pCredSN');
      (*N30*)NFe.Det[i].Imposto.ICMS.vCredICMSSN := Leitor.rCampo(tcDe2, 'vCredICMSSN');
      (*N31*)NFe.Det[i].Imposto.ICMS.vBCSTDest   := Leitor.rCampo(tcDe2, 'vBCSTDest');
      (*N32*)NFe.Det[i].Imposto.ICMS.vICMSSTDest := Leitor.rCampo(tcDe2, 'vICMSSTDest');
      (*N34*)NFe.Det[i].Imposto.ICMS.pRedBCEfet  := Leitor.rCampo(tcDe4, 'pRedBCEfet');
      (*N35*)NFe.Det[i].Imposto.ICMS.vBCEfet     := Leitor.rCampo(tcDe2, 'vBCEfet');
      (*N36*)NFe.Det[i].Imposto.ICMS.pICMSEfet   := Leitor.rCampo(tcDe4, 'pICMSEfet');
      (*N37*)NFe.Det[i].Imposto.ICMS.vICMSEfet   := Leitor.rCampo(tcDe2, 'vICMSEfet');
             NFe.Det[i].Imposto.ICMS.vICMSSTDeson := Leitor.rCampo(tcDe2, 'vICMSSTDeson');
             NFe.Det[i].Imposto.ICMS.motDesICMSST := StrTomotDesICMS(ok, Leitor.rCampo(tcStr, 'motDesICMSST'));
             NFe.Det[i].Imposto.ICMS.pFCPDif     := Leitor.rCampo(tcDe4, 'pFCPDif');
             NFe.Det[i].Imposto.ICMS.vFCPDif     := Leitor.rCampo(tcDe2, 'vFCPDif');
             NFe.Det[i].Imposto.ICMS.vFCPEfet    := Leitor.rCampo(tcDe2, 'vFCPEfet');

             NFe.Det[i].Imposto.ICMS.adRemICMS := Leitor.rCampo(tcDe4, 'adRemICMS');
             NFe.Det[i].Imposto.ICMS.vICMSMono := Leitor.rCampo(tcDe2, 'vICMSMono');
             NFe.Det[i].Imposto.ICMS.adRemICMSReten := Leitor.rCampo(tcDe4, 'adRemICMSReten');
             NFe.Det[i].Imposto.ICMS.vICMSMonoReten := Leitor.rCampo(tcDe2, 'vICMSMonoReten');
             NFe.Det[i].Imposto.ICMS.vICMSMonoDif := Leitor.rCampo(tcDe2, 'vICMSMonoDif');
             NFe.Det[i].Imposto.ICMS.adRemICMSRet := Leitor.rCampo(tcDe4, 'adRemICMSRet');
             NFe.Det[i].Imposto.ICMS.vICMSMonoRet := Leitor.rCampo(tcDe2, 'vICMSMonoRet');

             NFe.Det[i].Imposto.ICMS.qBCMono := Leitor.rCampo(tcDe4, 'qBCMono');
             NFe.Det[i].Imposto.ICMS.qBCMonoReten := Leitor.rCampo(tcDe4, 'qBCMonoReten');
             NFe.Det[i].Imposto.ICMS.pRedAdRem := Leitor.rCampo(tcDe4, 'pRedAdRem');

             if NFe.Det[i].Imposto.ICMS.pRedAdRem <> 0 then
               NFe.Det[i].Imposto.ICMS.motRedAdRem := StrTomotRedAdRem(ok, Leitor.rCampo(tcStr, 'motRedAdRem'));

             NFe.Det[i].Imposto.ICMS.qBCMonoRet := Leitor.rCampo(tcDe4, 'qBCMonoRet');
             NFe.Det[i].Imposto.ICMS.vICMSMonoOp := Leitor.rCampo(tcDe2, 'vICMSMonoOp');

      (*N26b*)NFe.Det[i].Imposto.ICMS.vICMSSubstituto := Leitor.rCampo(tcDe2, 'vICMSSubstituto');

             sAux := Leitor.rCampo(tcStr, 'indDeduzDeson');
             NFe.Det[i].Imposto.ICMS.indDeduzDeson := tiNao;
             if sAux = '1' then
               NFe.Det[i].Imposto.ICMS.indDeduzDeson := tiSim;

             NFe.Det[i].Imposto.ICMS.cBenefRBC := Leitor.rCampo(tcStr, 'cBenefRBC');

      if Leitor.rExtrai(4, 'ICMSPart') <> '' then
      begin
        case NFe.Det[i].Imposto.ICMS.CST of
          cst10 : NFe.Det[i].Imposto.ICMS.CST := cstPart10;
          cst90 : NFe.Det[i].Imposto.ICMS.CST := cstPart90;
        end;
      end
      else if Leitor.rExtrai(4, 'ICMSST') <> '' then
      begin
        case NFe.Det[i].Imposto.ICMS.CST of
          cst41 : NFe.Det[i].Imposto.ICMS.CST := cstRep41;
          cst60 : NFe.Det[i].Imposto.ICMS.CST := cstRep60;
        end;
      end;
    end;

    if Leitor.rExtrai(3, 'ICMSUFDest') <> '' then
    begin
      (*NA03*)NFe.Det[i].Imposto.ICMSUFDest.vBCUFDest      := Leitor.rCampo(tcDe2, 'vBCUFDest');
      (*NA04*)NFe.Det[i].Imposto.ICMSUFDest.vBCFCPUFDest   := Leitor.rCampo(tcDe2, 'vBCFCPUFDest');
      (*NA05*)NFe.Det[i].Imposto.ICMSUFDest.pFCPUFDest     := Leitor.rCampo(tcDe2, 'pFCPUFDest');
      (*NA07*)NFe.Det[i].Imposto.ICMSUFDest.pICMSUFDest    := Leitor.rCampo(tcDe2, 'pICMSUFDest');
      (*NA09*)NFe.Det[i].Imposto.ICMSUFDest.pICMSInter     := Leitor.rCampo(tcDe2, 'pICMSInter');
      (*NA11*)NFe.Det[i].Imposto.ICMSUFDest.pICMSInterPart := Leitor.rCampo(tcDe2, 'pICMSInterPart');
      (*NA13*)NFe.Det[i].Imposto.ICMSUFDest.vFCPUFDest     := Leitor.rCampo(tcDe2, 'vFCPUFDest');
      (*NA15*)NFe.Det[i].Imposto.ICMSUFDest.vICMSUFDest    := Leitor.rCampo(tcDe2, 'vICMSUFDest');
      (*NA17*)NFe.Det[i].Imposto.ICMSUFDest.vICMSUFRemet   := Leitor.rCampo(tcDe2, 'vICMSUFRemet');
    end;

    if Leitor.rExtrai(3, 'IPI') <> '' then
    begin
      (*O02*)NFe.Det[i].Imposto.IPI.clEnq    := Leitor.rCampo(tcStr, 'clEnq');
      (*O03*)NFe.Det[i].Imposto.IPI.CNPJProd := Leitor.rCampo(tcStr, 'CNPJProd');
      (*O04*)NFe.Det[i].Imposto.IPI.cSelo    := Leitor.rCampo(tcStr, 'cSelo');
      (*O05*)NFe.Det[i].Imposto.IPI.qSelo    := Leitor.rCampo(tcInt, 'qSelo');
      (*O06*)NFe.Det[i].Imposto.IPI.cEnq     := Leitor.rCampo(tcStr, 'cEnq');


      // Inicializa CST com sendo Não tributada e conforme o TIPO entrada ou saida
      // Caso a Tag não seja informada sera gravada com sendo não tributada
      if NFe.ide.tpNF = tnEntrada then
        NFe.Det[i].Imposto.IPI.CST := ipi53;
      if NFe.ide.tpNF = tnSaida then
        NFe.Det[i].Imposto.IPI.CST := ipi03;

      if Leitor.rExtrai(3, 'IPITrib') <> '' then
      begin
        (*O09*)NFe.Det[i].Imposto.IPI.CST   := StrToCSTIPI(ok, Leitor.rCampo(tcStr, 'CST'));
        (*O10*)NFe.Det[i].Imposto.IPI.vBC   := Leitor.rCampo(tcDe2, 'vBC');
        (*O11*)NFe.Det[i].Imposto.IPI.qUnid := Leitor.rCampo(tcDe4, 'qUnid');
        (*O12*)NFe.Det[i].Imposto.IPI.vUnid := Leitor.rCampo(tcDe4, 'vUnid');
        (*O13*)NFe.Det[i].Imposto.IPI.pIPI  := Leitor.rCampo(tcDe2, 'pIPI');
        (*O14*)NFe.Det[i].Imposto.IPI.vIPI  := Leitor.rCampo(tcDe2, 'vIPI');
      end;
      if Leitor.rExtrai(3, 'IPINT') <> '' then
      begin
        (*O09*)NFe.Det[i].Imposto.IPI.CST := StrToCSTIPI(ok, Leitor.rCampo(tcStr, 'CST'));
      end;
    end;
    if Leitor.rExtrai(3, 'II') <> '' then
    begin
      (*P02*)NFe.Det[i].Imposto.II.vBc      := Leitor.rCampo(tcDe2, 'vBC');
      (*P03*)NFe.Det[i].Imposto.II.vDespAdu := Leitor.rCampo(tcDe2, 'vDespAdu');
      (*P04*)NFe.Det[i].Imposto.II.vII      := Leitor.rCampo(tcDe2, 'vII');
      (*P05*)NFe.Det[i].Imposto.II.vIOF     := Leitor.rCampo(tcDe2, 'vIOF');
    end;
    if Leitor.rExtrai(3, 'PIS') <> '' then
    begin
      (*Q06*)NFe.Det[i].Imposto.PIS.CST       := StrToCSTPIS(ok, Leitor.rCampo(tcStr, 'CST'));
      (*Q07*)NFe.Det[i].Imposto.PIS.vBC       := Leitor.rCampo(tcDe2, 'vBC');
      (*Q08*)NFe.Det[i].Imposto.PIS.pPIS      := Leitor.rCampo(tcDe2, 'pPIS');
      (*Q09*)NFe.Det[i].Imposto.PIS.vPIS      := Leitor.rCampo(tcDe2, 'vPIS');
      (*Q10*)NFe.Det[i].Imposto.PIS.qBCProd   := Leitor.rCampo(tcDe4, 'qBCProd');
      (*Q11*)NFe.Det[i].Imposto.PIS.vAliqProd := Leitor.rCampo(tcDe4, 'vAliqProd');
    end;
    if Leitor.rExtrai(3, 'PISST') <> '' then
    begin
      (*R02*)NFe.Det[i].Imposto.PISST.vBc       := Leitor.rCampo(tcDe2, 'vBC');
      (*R03*)NFe.Det[i].Imposto.PISST.pPis      := Leitor.rCampo(tcDe2, 'pPIS');
      (*R04*)NFe.Det[i].Imposto.PISST.qBCProd   := Leitor.rCampo(tcDe4, 'qBCProd');
      (*R05*)NFe.Det[i].Imposto.PISST.vAliqProd := Leitor.rCampo(tcDe4, 'vAliqProd');
      (*R06*)NFe.Det[i].Imposto.PISST.vPIS      := Leitor.rCampo(tcDe2, 'vPIS');
      (*R07*)NFe.Det[i].Imposto.PISST.indSomaPISST := StrToindSomaPISST(ok, Leitor.rCampo(tcStr, 'indSomaPISST'));
    end;
    if Leitor.rExtrai(3, 'COFINS') <> '' then
    begin
      (*S06*)NFe.Det[i].Imposto.COFINS.CST       := StrToCSTCOFINS(ok, Leitor.rCampo(tcStr, 'CST'));
      (*S07*)NFe.Det[i].Imposto.COFINS.vBC       := Leitor.rCampo(tcDe2, 'vBC');
      (*S08*)NFe.Det[i].Imposto.COFINS.pCOFINS   := Leitor.rCampo(tcDe2, 'pCOFINS');
      (*S09*)NFe.Det[i].Imposto.COFINS.qBCProd   := Leitor.rCampo(tcDe4, 'qBCProd');
      (*S10*)NFe.Det[i].Imposto.COFINS.vAliqProd := Leitor.rCampo(tcDe4, 'vAliqProd');
      (*S11*)NFe.Det[i].Imposto.COFINS.vCOFINS   := Leitor.rCampo(tcDe2, 'vCOFINS');
    end;
    if Leitor.rExtrai(3, 'COFINSST') <> '' then
    begin
      (*T02*)NFe.Det[i].Imposto.COFINSST.vBC       := Leitor.rCampo(tcDe2, 'vBC');
      (*T03*)NFe.Det[i].Imposto.COFINSST.pCOFINS   := Leitor.rCampo(tcDe2, 'pCOFINS');
      (*T04*)NFe.Det[i].Imposto.COFINSST.qBCProd   := Leitor.rCampo(tcDe4, 'qBCProd');
      (*T05*)NFe.Det[i].Imposto.COFINSST.vAliqProd := Leitor.rCampo(tcDe4, 'vAliqProd');
      (*T06*)NFe.Det[i].Imposto.COFINSST.vCOFINS   := Leitor.rCampo(tcDe2, 'vCOFINS');
      (*T07*)NFe.Det[i].Imposto.COFINSST.indSomaCOFINSST := StrToindSomaCOFINSST(ok, Leitor.rCampo(tcStr, 'indSomaCOFINSST'));
    end;
    if Leitor.rExtrai(3, 'ISSQN') <> '' then
    begin
      (*U02*)NFe.Det[i].Imposto.ISSQN.vBC       := Leitor.rCampo(tcDe2, 'vBC');
      (*U03*)NFe.Det[i].Imposto.ISSQN.vAliq     := Leitor.rCampo(tcDe2, 'vAliq');
      (*U04*)NFe.Det[i].Imposto.ISSQN.vISSQN    := Leitor.rCampo(tcDe2, 'vISSQN');
      (*U05*)NFe.Det[i].Imposto.ISSQN.cMunFG    := Leitor.rCampo(tcInt, 'cMunFG');
      (*U06*)NFe.Det[i].Imposto.ISSQN.cListServ := Leitor.rCampo(tcStr, 'cListServ');
      (*U07*)NFe.Det[i].Imposto.ISSQN.cSitTrib  := StrToISSQNcSitTrib( ok,  Leitor.rCampo(tcStr, 'cSitTrib') ) ;

      (*U07*)NFe.Det[i].Imposto.ISSQN.vDeducao     := Leitor.rCampo(tcDe2, 'vDeducao');
      (*U08*)NFe.Det[i].Imposto.ISSQN.vOutro       := Leitor.rCampo(tcDe2, 'vOutro');
      (*U09*)NFe.Det[i].Imposto.ISSQN.vDescIncond  := Leitor.rCampo(tcDe2, 'vDescIncond');
      (*U10*)NFe.Det[i].Imposto.ISSQN.vDescCond    := Leitor.rCampo(tcDe2, 'vDescCond');
//      (*U11*)NFe.Det[i].Imposto.ISSQN.indISSRet    := StrToindISSRet(Ok, Leitor.rCampo(tcStr, 'indISSRet'));
      (*U12*)NFe.Det[i].Imposto.ISSQN.vISSRet      := Leitor.rCampo(tcDe2, 'vISSRet');
      (*U13*)NFe.Det[i].Imposto.ISSQN.indISS       := StrToindISS(Ok, Leitor.rCampo(tcStr, 'indISS'));
      (*U14*)NFe.Det[i].Imposto.ISSQN.cServico     := Leitor.rCampo(tcStr, 'cServico');
      (*U15*)NFe.Det[i].Imposto.ISSQN.cMun         := Leitor.rCampo(tcInt, 'cMun');
      (*U16*)NFe.Det[i].Imposto.ISSQN.cPais        := Leitor.rCampo(tcInt, 'cPais');
      (*U17*)NFe.Det[i].Imposto.ISSQN.nProcesso    := Leitor.rCampo(tcStr, 'nProcesso');
      (*U18*)NFe.Det[i].Imposto.ISSQN.indIncentivo := StrToindIncentivo(Ok, Leitor.rCampo(tcStr, 'indIncentivo'));
    end;

    (* Grupo da TAG <det><impostoDevol> *)
    if Leitor.rExtrai(2, 'impostoDevol') <> '' then
    begin
      (*U51*)NFe.Det[i].pDevol := Leitor.rCampo(tcDe2, 'pDevol');
      if Leitor.rExtrai(3, 'IPI') <> '' then
      begin
        (*U61*)NFe.Det[i].vIPIDevol := Leitor.rCampo(tcDe2, 'vIPIDevol');
      end;
    end;

    if Leitor.rExtrai(2, 'obsItem') <> '' then
    begin
      if Leitor.rExtrai(3, 'obsCont') <> '' then
      begin
        NFe.Det[i].obsCont.xCampo := Leitor.rAtributo('xCampo');
        NFe.Det[i].obsCont.xTexto := Leitor.rCampo(tcStr, 'xTexto');
      end;

      if Leitor.rExtrai(3, 'obsFisco') <> '' then
      begin
        NFe.Det[i].obsFisco.xCampo := Leitor.rAtributo('xCampo');
        NFe.Det[i].obsFisco.xTexto := Leitor.rCampo(tcStr, 'xTexto');
      end;
    end;

    inc(i);
  end;

  Leitor.Arquivo := Arquivo;

  (* Grupo da TAG <total> *****************************************************)
  if Leitor.rExtrai(1, 'total') <> '' then
  begin
    if Leitor.rExtrai(2, 'ICMSTot') <> '' then
    begin
      (*W03*)NFe.Total.ICMSTot.vBC           := Leitor.rCampo(tcDe2, 'vBC');
      (*W04*)NFe.Total.ICMSTot.vICMS         := Leitor.rCampo(tcDe2, 'vICMS');
      (*W04a*)NFe.Total.ICMSTot.vICMSDeson   := Leitor.rCampo(tcDe2, 'vICMSDeson');
      (*W04c*)NFe.Total.ICMSTot.vFCPUFDest   := Leitor.rCampo(tcDe2, 'vFCPUFDest');
      (*W04e*)NFe.Total.ICMSTot.vICMSUFDest  := Leitor.rCampo(tcDe2, 'vICMSUFDest');
      (*W04g*)NFe.Total.ICMSTot.vICMSUFRemet := Leitor.rCampo(tcDe2, 'vICMSUFRemet');
      (*W04h*)NFe.Total.ICMSTot.vFCP         := Leitor.rCampo(tcDe2, 'vFCP');
      (*W05*)NFe.Total.ICMSTot.vBCST         := Leitor.rCampo(tcDe2, 'vBCST');
      (*W06*)NFe.Total.ICMSTot.vST           := Leitor.rCampo(tcDe2, 'vST');
      (*W06a*)NFe.Total.ICMSTot.vFCPST       := Leitor.rCampo(tcDe2, 'vFCPST');
      (*W06b*)NFe.Total.ICMSTot.vFCPSTRet    := Leitor.rCampo(tcDe2, 'vFCPSTRet');

      NFe.Total.ICMSTot.qBCMono := Leitor.rCampo(tcDe2, 'qBCMono');
      NFe.Total.ICMSTot.vICMSMono := Leitor.rCampo(tcDe2, 'vICMSMono');
      NFe.Total.ICMSTot.qBCMonoReten := Leitor.rCampo(tcDe2, 'qBCMonoReten');
      NFe.Total.ICMSTot.vICMSMonoReten := Leitor.rCampo(tcDe2, 'vICMSMonoReten');
      NFe.Total.ICMSTot.qBCMonoRet := Leitor.rCampo(tcDe2, 'qBCMonoRet');
      NFe.Total.ICMSTot.vICMSMonoRet := Leitor.rCampo(tcDe2, 'vICMSMonoRet');

      (*W07*)NFe.Total.ICMSTot.vProd         := Leitor.rCampo(tcDe2, 'vProd');
      (*W08*)NFe.Total.ICMSTot.vFrete        := Leitor.rCampo(tcDe2, 'vFrete');
      (*W09*)NFe.Total.ICMSTot.vSeg          := Leitor.rCampo(tcDe2, 'vSeg');
      (*W10*)NFe.Total.ICMSTot.vDesc         := Leitor.rCampo(tcDe2, 'vDesc');
      (*W11*)NFe.Total.ICMSTot.vII           := Leitor.rCampo(tcDe2, 'vII');
      (*W12*)NFe.Total.ICMSTot.vIPI          := Leitor.rCampo(tcDe2, 'vIPI');
      (*W12a*)NFe.Total.ICMSTot.vIPIDevol    := Leitor.rCampo(tcDe2, 'vIPIDevol');
      (*W13*)NFe.Total.ICMSTot.vPIS          := Leitor.rCampo(tcDe2, 'vPIS');
      (*W14*)NFe.Total.ICMSTot.vCOFINS       := Leitor.rCampo(tcDe2, 'vCOFINS');
      (*W15*)NFe.Total.ICMSTot.vOutro        := Leitor.rCampo(tcDe2, 'vOutro');
      (*W16*)NFe.Total.ICMSTot.vNF           := Leitor.rCampo(tcDe2, 'vNF');
      (*W16a*)NFe.Total.ICMSTot.vTotTrib     := Leitor.rCampo(tcDe2, 'vTotTrib');
    end;
    if Leitor.rExtrai(2, 'ISSQNtot') <> '' then
    begin
      (*W18*)NFe.Total.ISSQNtot.vServ   := Leitor.rCampo(tcDe2, 'vServ');
      (*W19*)NFe.Total.ISSQNtot.vBC     := Leitor.rCampo(tcDe2, 'vBC');
      (*W20*)NFe.Total.ISSQNtot.vISS    := Leitor.rCampo(tcDe2, 'vISS');
      (*W21*)NFe.Total.ISSQNtot.vPIS    := Leitor.rCampo(tcDe2, 'vPIS');
      (*W22*)NFe.Total.ISSQNtot.vCOFINS := Leitor.rCampo(tcDe2, 'vCOFINS');

      if NFe.infNFe.Versao >= 3 then
      begin
        (*W22a*)NFe.Total.ISSQNtot.dCompet     := Leitor.rCampo(tcDat, 'dCompet');
        (*W22b*)NFe.Total.ISSQNtot.vDeducao    := Leitor.rCampo(tcDe2, 'vDeducao');
        (*W22c*)NFe.Total.ISSQNtot.vOutro      := Leitor.rCampo(tcDe2, 'vOutro');
        (*W22d*)NFe.Total.ISSQNtot.vDescIncond := Leitor.rCampo(tcDe2, 'vDescIncond');
        (*W22e*)NFe.Total.ISSQNtot.vDescCond   := Leitor.rCampo(tcDe2, 'vDescCond');
        (*W22f*)NFe.Total.ISSQNtot.vISSRet     := Leitor.rCampo(tcDe2, 'vISSRet');

        sAux := Leitor.rCampo(tcStr, 'cRegTrib');
        NFe.Total.ISSQNtot.cRegTrib := RTISSNenhum;
        if sAux <> '' then
          (*W22g*)NFe.Total.ISSQNtot.cRegTrib := StrToRegTribISSQN(Ok, sAux);
      end;
    end;
    
    if Leitor.rExtrai(2, 'retTrib') <> '' then
    begin
      (*W24*)NFe.Total.retTrib.vRetPIS    := Leitor.rCampo(tcDe2, 'vRetPIS');
      (*W25*)NFe.Total.retTrib.vRetCOFINS := Leitor.rCampo(tcDe2, 'vRetCOFINS');
      (*W26*)NFe.Total.retTrib.vRetCSLL   := Leitor.rCampo(tcDe2, 'vRetCSLL');
      (*W27*)NFe.Total.retTrib.vBCIRRF    := Leitor.rCampo(tcDe2, 'vBCIRRF');
      (*W28*)NFe.Total.retTrib.vIRRF      := Leitor.rCampo(tcDe2, 'vIRRF');
      (*W29*)NFe.Total.retTrib.vBCRetPrev := Leitor.rCampo(tcDe2, 'vBCRetPrev');
      (*W30*)NFe.Total.retTrib.vRetPrev   := Leitor.rCampo(tcDe2, 'vRetPrev');
    end;
  end;

  (* Grupo da TAG <transp> ****************************************************)
  if Leitor.rExtrai(1, 'transp') <> '' then
  begin
    (*X02*)NFe.Transp.modFrete := StrToModFrete(ok, Leitor.rCampo(tcStr, 'modFrete'));
    (*X25a*)NFe.Transp.vagao   := Leitor.rCampo(tcStr, 'vagao');
    (*X25b*)NFe.Transp.balsa   := Leitor.rCampo(tcStr, 'balsa');
    if Leitor.rExtrai(2, 'transporta') <> '' then
    begin
      (*X04/X05*)NFe.Transp.Transporta.CNPJCPF := Leitor.rCampoCNPJCPF;
      (*X06*)NFe.Transp.Transporta.xNome       := Leitor.rCampo(tcStr, 'xNome');
      (*X07*)NFe.Transp.Transporta.IE          := Leitor.rCampo(tcStr, 'IE');
      (*X08*)NFe.Transp.Transporta.xEnder      := Leitor.rCampo(tcStr, 'xEnder');
      (*X09*)NFe.Transp.Transporta.xMun        := Leitor.rCampo(tcStr, 'xMun');
      (*X10*)NFe.Transp.Transporta.UF          := Leitor.rCampo(tcStr, 'UF');
    end;
    if Leitor.rExtrai(2, 'retTransp') <> '' then
    begin
      (*X12*)NFe.Transp.retTransp.vServ    := Leitor.rCampo(tcDe2, 'vServ');
      (*X13*)NFe.Transp.retTransp.vBCRet   := Leitor.rCampo(tcDe2, 'vBCRet');
      (*X14*)NFe.Transp.retTransp.pICMSRet := Leitor.rCampo(tcDe2, 'pICMSRet');
      (*X15*)NFe.Transp.retTransp.vICMSRet := Leitor.rCampo(tcDe2, 'vICMSRet');
      (*X16*)NFe.Transp.retTransp.CFOP     := Leitor.rCampo(tcEsp, 'CFOP');
      (*X17*)NFe.Transp.retTransp.cMunFG   := Leitor.rCampo(tcStr, 'cMunFG');
    end;
    if Leitor.rExtrai(2, 'veicTransp') <> '' then
    begin
      (*X19*)NFe.Transp.veicTransp.placa := Leitor.rCampo(tcStr, 'placa');
      (*X20*)NFe.Transp.veicTransp.UF    := Leitor.rCampo(tcStr, 'UF');
      (*X21*)NFe.Transp.veicTransp.RNTC  := Leitor.rCampo(tcStr, 'RNTC');
    end;

    i := 0;
    NFe.Transp.Reboque.Clear;
    while Leitor.rExtrai(2, 'reboque', '', i + 1) <> '' do
    begin
      NFe.Transp.Reboque.New;
      (*X23*) NFe.Transp.Reboque[i].placa := Leitor.rCampo(tcStr, 'placa');
      (*X24*) NFe.Transp.Reboque[i].UF    := Leitor.rCampo(tcStr, 'UF');
      (*X25*) NFe.Transp.Reboque[i].RNTC  := Leitor.rCampo(tcStr, 'RNTC');
      inc(i);
    end;

    i := 0;
    NFe.Transp.Vol.Clear;
    while Leitor.rExtrai(2, 'vol', '', i + 1) <> '' do
    begin
      NFe.Transp.Vol.New;
      (*X27*)NFe.Transp.Vol[i].qVol  := Leitor.rCampo(tcInt, 'qVol');
      (*X28*)NFe.Transp.vol[i].esp   := Leitor.rCampo(tcStr, 'esp');
      (*X29*)NFe.Transp.Vol[i].marca := Leitor.rCampo(tcStr, 'marca');
      (*X30*)NFe.Transp.Vol[i].nVol  := Leitor.rCampo(tcStr, 'nVol');
      (*X31*)NFe.Transp.Vol[i].pesoL := Leitor.rCampo(tcDe3, 'pesoL');
      (*X32*)NFe.Transp.Vol[i].pesoB := Leitor.rCampo(tcDe3, 'pesoB');
      j := 0;
      NFe.transp.Vol[i].lacres.Clear;
      while Leitor.rExtrai(3, 'lacres', '', j + 1) <> '' do
      begin
        NFe.transp.Vol[i].lacres.New;
        (*X34*)NFe.transp.Vol[i].lacres[j].nLacre := Leitor.rCampo(tcStr, 'nLacre');
        inc(j);
      end;
      inc(i);
    end;

  end;

  (* Grupo da TAG <cobr> ******************************************************)
  if Leitor.rExtrai(1, 'cobr') <> '' then
  begin
    if Leitor.rExtrai(1, 'fat') <> '' then
    begin
      (*Y03*)NFe.Cobr.Fat.nFat  := Leitor.rCampo(tcStr, 'nFat');
      (*Y04*)NFe.Cobr.Fat.vOrig := Leitor.rCampo(tcDe2, 'vOrig');
      (*Y05*)NFe.Cobr.Fat.vDesc := Leitor.rCampo(tcDe2, 'vDesc');
      (*Y06*)NFe.Cobr.Fat.vLiq  := Leitor.rCampo(tcDe2, 'vLiq');
    end;
    i := 0;
    NFe.Cobr.Dup.Clear;
    while Leitor.rExtrai(1, 'dup', '', i + 1) <> '' do
    begin
      NFe.Cobr.Dup.New;
      (*Y08*)NFe.Cobr.Dup[i].nDup  := Leitor.rCampo(tcStr, 'nDup');
      (*Y09*)NFe.Cobr.Dup[i].dVenc := Leitor.rCampo(tcDat, 'dVenc');
      (*Y10*)NFe.Cobr.Dup[i].vDup  := Leitor.rCampo(tcDe2, 'vDup');
      inc(i);
    end;
  end;

  if NFe.infNFe.Versao >= 3 then
   begin
    (* Grupo da TAG <pag> ******************************************************)
    i := 0;
    NFe.pag.Clear;
    if NFe.infNFe.Versao >= 4 then
    begin
      if Leitor.rExtrai(1, 'pag') <> '' then
        (*YA09*)NFe.pag.vTroco := Leitor.rCampo(tcDe2, 'vTroco');
      tagPag := 'detPag';
    end
    else
      tagPag := 'pag';

    while Leitor.rExtrai(1, tagPag, '', i + 1) <> '' do
     begin
       NFe.pag.New;
      (*YA01b*)NFe.pag[i].indPag := StrToIndpag(Ok, Leitor.rCampo(tcStr, 'indPag'));
      (*YA02*)NFe.pag[i].tPag := StrToFormaPagamento(ok, Leitor.rCampo(tcStr, 'tPag'));
              NFe.pag[i].xPag := Leitor.rCampo(tcStr, 'xPag');
      (*YA03*)NFe.pag[i].vPag := Leitor.rCampo(tcDe2, 'vPag');
              NFe.pag[i].dPag := Leitor.rCampo(tcDat, 'dPag');

              NFe.pag[i].CNPJPag := Leitor.rCampo(tcStr, 'CNPJPag');
              NFe.pag[i].UFPag := Leitor.rCampo(tcStr, 'UFPag');

      if Leitor.rExtrai(2, 'card') <> '' then
       begin
        (*YA04a*)NFe.pag[i].tpIntegra := StrTotpIntegra(ok, Leitor.rCampo(tcStr, 'tpIntegra'));
        (*YA05*)NFe.pag[i].CNPJ  := Leitor.rCampo(tcStr, 'CNPJ');
        (*YA06*)NFe.pag[i].tBand := StrToBandeiraCartao(ok, Leitor.rCampo(tcStr, 'tBand'));
        (*YA07*)NFe.pag[i].cAut  := Leitor.rCampo(tcStr, 'cAut');

                NFe.pag[i].CNPJReceb := Leitor.rCampo(tcStr, 'CNPJReceb');
                NFe.pag[i].idTermPag := Leitor.rCampo(tcStr, 'idTermPag');
       end;
      inc(i);
    end;
   end;

  if Leitor.rExtrai(1, 'infIntermed') <> '' then
  begin
    NFe.infIntermed.CNPJ         := Leitor.rCampo(tcStr, 'CNPJ');
    NFe.infIntermed.idCadIntTran := Leitor.rCampo(tcStr, 'idCadIntTran');
  end;

  (* Grupo da TAG <InfAdic> ***************************************************)

  if Leitor.rExtrai(1, 'infAdic') <> '' then
  begin
    (*Z02*)NFe.InfAdic.infAdFisco := Leitor.rCampo(tcStr, 'infAdFisco');
    (*Z03*)NFe.InfAdic.infCpl     := Leitor.rCampo(tcStr, 'infCpl');
    i := 0;
    NFe.InfAdic.obsCont.Clear;
    while Leitor.rExtrai(2, 'obsCont', '', i + 1) <> '' do
    begin
      NFe.InfAdic.obsCont.New;
      (*Z05*)NFe.InfAdic.obsCont[i].xCampo := Leitor.rAtributo('xCampo');
      (*Z06*)NFe.InfAdic.obsCont[i].xTexto := Leitor.rCampo(tcStr, 'xTexto');
      inc(i);
    end;
    i := 0;
    NFe.InfAdic.obsFisco.Clear;
    while Leitor.rExtrai(2, 'obsFisco', '', i + 1) <> '' do
    begin
      NFe.InfAdic.obsFisco.New;
      (*Z08*)NFe.InfAdic.obsFisco[i].xCampo := Leitor.rAtributo('xCampo');
      (*Z09*)NFe.InfAdic.obsFisco[i].xTexto := Leitor.rCampo(tcStr, 'xTexto');
      inc(i)
    end;
    i := 0;
    NFe.InfAdic.procRef.Clear;
    while Leitor.rExtrai(2, 'procRef', '', i + 1) <> '' do
    begin
      NFe.InfAdic.procRef.New;
      (*Z11*)NFe.InfAdic.procRef[i].nProc := Leitor.rCampo(tcStr, 'nProc');
      (*Z12*)NFe.InfAdic.procRef[i].indProc := StrToIndProc(ok, Leitor.rCampo(tcStr, 'indProc'));
      (*Z13*)NFe.InfAdic.procRef[i].tpAto := StrTotpAto(ok, Leitor.rCampo(tcStr, 'tpAto'));
      inc(i);
    end;
  end;

  (* Grupo da TAG <exporta> ***************************************************)
  if Leitor.rExtrai(1, 'exporta') <> '' then
  begin
    (*ZA02*)NFe.exporta.UFembarq   := Leitor.rCampo(tcStr, 'UFEmbarq');
    (*ZA03*)NFe.exporta.xLocEmbarq := Leitor.rCampo(tcStr, 'xLocEmbarq');

    // Versao 3.10
    (*ZA02*)NFe.exporta.UFSaidaPais  := Leitor.rCampo(tcStr, 'UFSaidaPais');
    (*ZA03*)NFe.exporta.xLocExporta  := Leitor.rCampo(tcStr, 'xLocExporta');
    (*ZA04*)NFe.exporta.xLocDespacho := Leitor.rCampo(tcStr, 'xLocDespacho');
  end;

  (* Grupo da TAG <compra> ****************************************************)
  if Leitor.rExtrai(1, 'compra') <> '' then
  begin
    (*ZB02*)NFe.compra.xNEmp := Leitor.rCampo(tcStr, 'xNEmp');
    (*ZB03*)NFe.compra.xPed  := Leitor.rCampo(tcStr, 'xPed');
    (*ZB04*)NFe.compra.xCont := Leitor.rCampo(tcStr, 'xCont');
  end;

  (* Grupo da TAG <cana> ****************************************************)
  if Leitor.rExtrai(1, 'cana') <> '' then
  begin
    (*ZC02*) NFe.cana.safra   := Leitor.rCampo(tcStr, 'safra');
    (*ZC03*) NFe.cana.ref     := Leitor.rCampo(tcStr, 'ref');
    (*ZC07*) NFe.cana.qTotMes := Leitor.rCampo(tcDe10, 'qTotMes');
    (*ZC08*) NFe.cana.qTotAnt := Leitor.rCampo(tcDe10, 'qTotAnt');
    (*ZC09*) NFe.cana.qTotGer := Leitor.rCampo(tcDe10, 'qTotGer');
    (*ZC13*) NFe.cana.vFor    := Leitor.rCampo(tcDe2, 'vFor');
    (*ZC14*) NFe.cana.vTotDed := Leitor.rCampo(tcDe2, 'vTotDed');
    (*ZC15*) NFe.cana.vLiqFor := Leitor.rCampo(tcDe2, 'vLiqFor');

    i := 0;
    NFe.cana.fordia.Clear;
    while Leitor.rExtrai(2, 'forDia', '', i + 1) <> '' do
    begin
      NFe.cana.fordia.New;
      (*ZC05*) NFe.cana.fordia[i].dia  := Leitor.rAtributo('dia');
      (*ZC06*) NFe.cana.fordia[i].qtde := Leitor.rCampo(tcDe10, 'qtde');
      inc(i);
    end;

    i := 0;
    NFe.cana.deduc.Clear;
    while Leitor.rExtrai(2, 'deduc', '', i + 1) <> '' do
    begin
      NFe.cana.deduc.New;
      (*ZC11*) NFe.cana.deduc[i].xDed := Leitor.rCampo(tcStr, 'xDed');
      (*ZC12*) NFe.cana.deduc[i].vDed := Leitor.rCampo(tcDe2, 'vDed');
      inc(i);
    end;

  end;

  if Leitor.rExtrai(1, 'infRespTec') <> '' then
  begin
    NFe.infRespTec.CNPJ     := Leitor.rCampo(tcStr, 'CNPJ');
    NFe.infRespTec.xContato := Leitor.rCampo(tcStr, 'xContato');
    NFe.infRespTec.email    := Leitor.rCampo(tcStr, 'email');
    NFe.infRespTec.fone     := Leitor.rCampo(tcStr, 'fone');
    NFe.infRespTec.idCSRT   := Leitor.rCampo(tcInt, 'idCSRT');
    NFe.infRespTec.hashCSRT := Leitor.rCampo(tcStr, 'hashCSRT');
  end;

  (* Grupo da TAG <signature> *************************************************)

  leitor.Grupo := Leitor.Arquivo;

  NFe.signature.URI             := Leitor.rAtributo('Reference URI=');
  NFE.signature.DigestValue     := Leitor.rCampo(tcStr, 'DigestValue');
  NFE.signature.SignatureValue  := Leitor.rCampo(tcStr, 'SignatureValue');
  NFE.signature.X509Certificate := Leitor.rCampo(tcStr, 'X509Certificate');

  (* Grupo da TAG <infNFeSupl> ************************************************)
  if Leitor.rExtrai(1, 'infNFeSupl') <> '' then
  begin
    NFe.infNFeSupl.qrCode := Leitor.rCampo(tcStr, 'qrCode');
    NFe.infNFeSupl.qrCode := StringReplace(NFe.infNFeSupl.qrCode, '<![CDATA[', '', []);
    NFe.infNFeSupl.qrCode := StringReplace(NFe.infNFeSupl.qrCode, ']]>', '', []);
    NFe.infNFeSupl.urlChave := Leitor.rCampo(tcStr, 'urlChave');
  end;

  (* Grupo da TAG <protNFe> ***************************************************)
  if Leitor.rExtrai(1, 'protNFe') <> '' then
  begin
    NFe.procNFe.tpAmb    := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
    NFe.procNFe.verAplic := Leitor.rCampo(tcStr, 'verAplic');
    NFe.procNFe.chNFe    := Leitor.rCampo(tcStr, 'chNFe');
    NFe.procNFe.dhRecbto := Leitor.rCampo(tcDatHor, 'dhRecbto');
    NFe.procNFe.nProt    := Leitor.rCampo(tcStr, 'nProt');
    NFe.procNFe.digVal   := Leitor.rCampo(tcStr, 'digVal');
    NFe.procNFe.cStat    := Leitor.rCampo(tcInt, 'cStat');
    NFe.procNFe.xMotivo  := Leitor.rCampo(tcStr, 'xMotivo');
    NFe.procNFe.cMsg     := Leitor.rCampo(tcInt, 'cMsg');
    NFe.procNFe.xMsg     := Leitor.rCampo(tcStr, 'xMsg');
  end;

  Result := true;

end;

end.

