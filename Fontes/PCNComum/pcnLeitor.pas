////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar NFe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml da NFe          //
//                                                                            //
//        site: www.projetocooperar.org/nfe                                   //
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
|* 30/09/2010: Italo
|*  - Incluído tipo TcDe6
******************************************************************************}

{$I ACBr.inc}

unit pcnLeitor;

interface

uses
  SysUtils, Classes,
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  pcnAuxiliar, pcnConversao;

type

  TLeitor = class(TPersistent)
  private
    FArquivo: String;
    FGrupo: String;
    FNivel: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function rExtrai(const nivel: integer; const TagInicio: string; TagFim: string = ''; const item: integer = 1): String;
    function rCampo(const Tipo: TpcnTipoCampo; TAG: string; const TAGparada: string = ''): variant;
    function rCampoCNPJCPF(const TAGparada: string = ''): string;
    function rAtributo(Atributo: string; Tag: String = ''): variant;
    function CarregarArquivo(const CaminhoArquivo: string): boolean; overload;
    function CarregarArquivo(const Stream: TStringStream): boolean; overload;
    function PosLast(const SubStr, S: String ): Integer;
  published
    property Arquivo: String read FArquivo write FArquivo;
    property Grupo: String read FGrupo write FGrupo;
  end;

implementation

uses
  ACBrUtil, StrUtils;

{ TLeitor }

constructor TLeitor.Create;
var
  i: integer;
begin
  inherited Create;
  FNivel := TStringList.Create;
  for i := 1 to 11 do
    FNivel.add('');
end;

destructor TLeitor.Destroy;
begin
  FNivel.Free;
  inherited;
end;

function TLeitor.CarregarArquivo(const CaminhoArquivo: string): boolean;
var
  ArquivoXML: TStringList;
begin
  //NOTA: Carrega o arquivo xml na memória para posterior leitura de sua tag's

  ArquivoXML := TStringList.Create;
  try
    ArquivoXML.LoadFromFile(CaminhoArquivo);
    FArquivo := ArquivoXML.Text;
    Result := True;
  finally
    ArquivoXML.Free;
  end;
end;

function TLeitor.CarregarArquivo(const Stream: TStringStream): boolean;
begin
  //NOTA: Carrega o arquivo xml na memória para posterior leitura de sua tag's
  FArquivo := Stream.DataString;
  Result := True;
end;

function TLeitor.rExtrai(const nivel: integer; const TagInicio: string; TagFim: string = ''; const item: integer = 1): String;
var
  Texto: String;
  i,j: integer;
begin
  //NOTA: Extrai um grupo de dentro do nivel informado
  FNivel.strings[0] := FArquivo;
  if Trim(TagFim) = '' then
    TagFim := TagInicio;
  Texto := FNivel.Strings[nivel - 1];
  Result := '';
  FGrupo := '';
  for i := 1 to item do
    if i < item then
      Texto := copy(Texto, pos('</' + Trim(TagFim) + '>', Texto) + length(Trim(TagFim)) + 3, maxInt);

  j := pos('</' + Trim(TagFim) + '>', Texto);
  if j = 0 then
    j := pos('</' + Trim(TagFim) + ':', Texto); // Correção para WebServices do Ceará/MG

  //Correção para leitura de tags em que a primeira é diferente da segunda Ex: <infProt id=XXX> e a segunda apenas <infProt>
//  Texto := copy(Texto, 1, pos('</' + Trim(TagFim) + '>', Texto) + length(Trim(TagFim)) + 3);
  Texto := copy(Texto, 1, j + length(Trim(TagFim)) + 3);

  i := pos('<' + Trim(TagInicio) + '>', Texto);
  if i = 0 then
    i := pos('<' + Trim(TagInicio) + ' ', Texto);
  if i = 0 then
    i := pos('<' + Trim(TagInicio) + ':', Texto); //correção para webservice do Ceará
  if i = 0 then
    exit;
  Texto := copy(Texto, i, maxInt);

  // Alterado por Claudemir em 13/03/2013: j:=pos('</' + Trim(TagFim) + '>',Texto);
//  j:=pos('</' + Trim(TagFim) + '>', Texto) + length(Trim(TagFim)) + 3;
  j:=pos('</' + Trim(TagFim) + '>', Texto);

  if j=0 then
   j:=pos('</' + Trim(TagFim) + ':', Texto); //correção para webservice do Ceará

//  Result := TrimRight(copy(Texto, 1, j - 1));
  Result := TrimRight(copy(Texto, 1, j - 1 + (length(Trim(TagFim)) + 3)));
  FNivel.strings[nivel] := Result;
  FGrupo := result;
end;

function TLeitor.rCampoCNPJCPF(const TAGparada: string = ''): string;
begin
  result := rCampo(tcStr, 'CNPJ', TAGparada);
  if trim(result) = '' then
    result := rCampo(tcStr, 'CPF', TAGparada);
end;

function TLeitor.rCampo(const Tipo: TpcnTipoCampo; TAG: string; const TAGparada: string = ''): variant;
var
  ConteudoTag: string;
  inicio, fim, inicioTAGparada: integer;
begin
  Tag := UpperCase(Trim(TAG));
  inicio := pos('<' + Tag + '>', UpperCase(FGrupo));

  if Trim(TAGparada) <> '' then
   begin
    inicioTAGparada := pos('<' + UpperCase(Trim(TAGparada)) + '>', UpperCase(FGrupo));
    if inicioTAGparada = 0 then
      inicioTAGparada := inicio;
   end
   else
    inicioTAGparada := inicio;

  if (inicio = 0) or (InicioTAGparada < inicio) then
    ConteudoTag := ''
  else
  begin
    inicio := inicio + Length(Tag) + 2;
    fim := pos('</' + Tag + '>', UpperCase(FGrupo)) - inicio;
    ConteudoTag := trim(copy(FGrupo, inicio, fim));
  end;
  case Tipo of
    tcStr     : result := ReverterFiltroTextoXML(ConteudoTag);
    tcDat     : begin
                  if length(ConteudoTag)>0 then
                    result := EncodeDate(StrToInt(copy(ConteudoTag, 01, 4)), StrToInt(copy(ConteudoTag, 06, 2)), StrToInt(copy(ConteudoTag, 09, 2)))
                  else
                    result:=0;
                  end;
    tcDatVcto : begin
                  if length(ConteudoTag)>0 then
                    result := EncodeDate(StrToInt(copy(ConteudoTag, 07, 4)), StrToInt(copy(ConteudoTag, 04, 2)), StrToInt(copy(ConteudoTag, 01, 2)))
                  else
                    Result:= 0;
                end;
    tcDatCFe  : begin
                  if length(ConteudoTag)>0 then
                    result := EncodeDate(StrToInt(copy(ConteudoTag, 01, 4)), StrToInt(copy(ConteudoTag, 05, 2)), StrToInt(copy(ConteudoTag, 07, 2)))
                  else
                    result:=0;
                end;
    tcDatHor  : begin
                  if length(ConteudoTag)>0 then
                    result := EncodeDate(StrToInt(copy(ConteudoTag, 01, 4)), StrToInt(copy(ConteudoTag, 06, 2)), StrToInt(copy(ConteudoTag, 09, 2))) +
                      EncodeTime(StrToInt(copy(ConteudoTag, 12, 2)), StrToInt(copy(ConteudoTag, 15, 2)), StrToInt(copy(ConteudoTag, 18, 2)), 0)
                  else
                    result:=0;
                end;
    tcHor     : begin
                  if length(ConteudoTag)>0 then
                    result := EncodeTime(StrToInt(copy(ConteudoTag, 1, 2)), StrToInt(copy(ConteudoTag, 4, 2)), StrToInt(copy(ConteudoTag, 7, 2)), 0)
                  else
                    result:=0;
                end;
    tcHorCFe  : begin
                  if length(ConteudoTag)>0 then
                    result := EncodeTime(StrToInt(copy(ConteudoTag, 1, 2)), StrToInt(copy(ConteudoTag, 3, 2)), StrToInt(copy(ConteudoTag, 5, 2)), 0)
                  else
                    result:=0;
                end;
    tcDatHorCFe : begin
                  if length(ConteudoTag)>0 then
                    result := EncodeDate(StrToInt(copy(ConteudoTag, 01, 4)), StrToInt(copy(ConteudoTag, 05, 2)), StrToInt(copy(ConteudoTag, 07, 2)))+
                      EncodeTime(StrToInt(copy(ConteudoTag, 09, 2)), StrToInt(copy(ConteudoTag, 11, 2)), StrToInt(copy(ConteudoTag, 13, 2)), 0)
                  else
                    result:=0;
                end;
    tcDe2,
    tcDe3,
    tcDe4,
    tcDe6,
    tcDe10    : begin
                  if length(ConteudoTag)>0 then
                    result := StringToFloatDef(ConteudoTag, 0)
                  else
                    result := 0;
                end;
    tcEsp     : result := ConteudoTag;
    tcInt     : begin
                  if length(ConteudoTag)>0 then
                    result := StrToIntDef(Trim(OnlyNumber(ConteudoTag)),0)
                  else
                    result := 0;
                end;
    else
      raise Exception.Create('Tag <' + Tag + '> com conteúdo inválido. '+ConteudoTag);
  end;
end;

function TLeitor.rAtributo(Atributo: string; Tag: String): variant;
var
  ConteudoTag ,
  Aspas       : String;

  inicio      ,
  inicioTag   ,
  fim         ,
  iPos1       ,
  iPos2       : Integer;

begin
  Result := '';
  Atributo := Trim(Atributo);
  Tag := Trim(Tag);
  inicioTag := pos(Tag, FGrupo);
  inicio := pos(Atributo, FGrupo);

  // se inicioTag > 0 significa que o parâmetro Tag foi informado.
  // se inicioTag > inicio significa que o atributo encontrado não é da Tag informada
  // logo devemos bustar a proxima ocorrecia a partir da posição da Tag.
  if (inicioTag > 0) and (inicioTag > inicio)  then
    inicio := PosEx(Atributo, FGrupo, inicioTag);

  if inicio > 0 then
  begin
    inicio := inicio + Length(Atributo);
    ConteudoTag := trim(copy(FGrupo, inicio, maxInt));

    iPos1 := Pos('"' , ConteudoTag);
    iPos2 := Pos('''', ConteudoTag);

    if iPos1 <> 0 then
      Aspas := '"';

    if (iPos2 <> 0) and ((iPos1 = 0) or (iPos2 < iPos1)) then
      Aspas := '''';

    inicio := pos(Aspas, ConteudoTag) + 1;
    if inicio > 0 then
    begin
      ConteudoTag := trim(copy(ConteudoTag, inicio, maxInt));
      fim := pos(Aspas, ConteudoTag) - 1;
      if fim > 0 then
      begin
        ConteudoTag := copy(ConteudoTag, 1, fim);
        result := ReverterFiltroTextoXML(ConteudoTag)
      end
    end ;
  end ;
end;

function TLeitor.PosLast(const SubStr, S: String ): Integer;
Var P : Integer ;
begin
  Result := 0 ;
  P := Pos( SubStr, S) ;
  while P <> 0 do
  begin
     Result := P ;
     P := RetornarPosEx( SubStr, S, P+1) ;
  end ;
end ;

end.

