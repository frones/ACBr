{This file generated automatically from libxslt-api.xml}
{For libxslt version: 1.1.24}
{$I ACBr.inc}

Unit libxslt;

interface

{$ALIGN 8}
{$MINENUMSIZE 4}

uses libxml2;

procedure Init;

const
{$IFDEF MSWINDOWS}
  {$IFDEF USE_MINGW}
    LIBXSLT_SO = 'libxslt-1.dll';
  {$ELSE}
    LIBXSLT_SO = 'libxslt.dll';
  {$ENDIF}
{$ELSE}
  LIBXSLT_SO = 'libxslt.so';
{$ENDIF}

type


      xsltTemplatePtrPtr = ^xsltTemplatePtr;
      xsltStackElemPtrPtr = ^xsltStackElemPtr;

          xsltDebugStatusCodes = (
          XSLT_DEBUG_NONE = 0,
          XSLT_DEBUG_INIT = 1,
          XSLT_DEBUG_STEP = 2,
          XSLT_DEBUG_STEPOUT = 3,
          XSLT_DEBUG_NEXT = 4,
          XSLT_DEBUG_STOP = 5,
          XSLT_DEBUG_CONT = 6,
          XSLT_DEBUG_RUN = 7,
          XSLT_DEBUG_RUN_RESTART = 8,
          XSLT_DEBUG_QUIT = 9);

      xsltDebugTraceCodes = (
          XSLT_TRACE_ALL = -1,
          XSLT_TRACE_NONE = 0,
          XSLT_TRACE_COPY_TEXT = 1,
          XSLT_TRACE_PROCESS_NODE = 2,
          XSLT_TRACE_APPLY_TEMPLATE = 4,
          XSLT_TRACE_COPY = 8,
          XSLT_TRACE_COMMENT = 16,
          XSLT_TRACE_PI = 32,
          XSLT_TRACE_COPY_OF = 64,
          XSLT_TRACE_VALUE_OF = 128,
          XSLT_TRACE_CALL_TEMPLATE = 256,
          XSLT_TRACE_APPLY_TEMPLATES = 512,
          XSLT_TRACE_CHOOSE = 1024,
          XSLT_TRACE_IF = 2048,
          XSLT_TRACE_FOR_EACH = 4096,
          XSLT_TRACE_STRIP_SPACES = 8192,
          XSLT_TRACE_TEMPLATES = 16384,
          XSLT_TRACE_KEYS = 32768,
          XSLT_TRACE_VARIABLES = 65536);

      xsltErrorSeverityType = (
          XSLT_ERROR_SEVERITY_ERROR = 0,
          XSLT_ERROR_SEVERITY_WARNING = 1);

      xsltLoadType = (
          XSLT_LOAD_START = 0,
          XSLT_LOAD_STYLESHEET = 1,
          XSLT_LOAD_DOCUMENT = 2);

      xsltOutputType = (
          XSLT_OUTPUT_XML = 0,
          XSLT_OUTPUT_HTML = 1,
          XSLT_OUTPUT_TEXT = 2);

      xsltSecurityOption = (
          XSLT_SECPREF_READ_FILE = 1,
          XSLT_SECPREF_WRITE_FILE = 2,
          XSLT_SECPREF_CREATE_DIRECTORY = 3,
          XSLT_SECPREF_READ_NETWORK = 4,
          XSLT_SECPREF_WRITE_NETWORK = 5);

      xsltStyleType = (
          XSLT_FUNC_COPY = 1,
          XSLT_FUNC_SORT = 2,
          XSLT_FUNC_TEXT = 3,
          XSLT_FUNC_ELEMENT = 4,
          XSLT_FUNC_ATTRIBUTE = 5,
          XSLT_FUNC_COMMENT = 6,
          XSLT_FUNC_PI = 7,
          XSLT_FUNC_COPYOF = 8,
          XSLT_FUNC_VALUEOF = 9,
          XSLT_FUNC_NUMBER = 10,
          XSLT_FUNC_APPLYIMPORTS = 11,
          XSLT_FUNC_CALLTEMPLATE = 12,
          XSLT_FUNC_APPLYTEMPLATES = 13,
          XSLT_FUNC_CHOOSE = 14,
          XSLT_FUNC_IF = 15,
          XSLT_FUNC_FOREACH = 16,
          XSLT_FUNC_DOCUMENT = 17,
          XSLT_FUNC_WITHPARAM = 18,
          XSLT_FUNC_PARAM = 19,
          XSLT_FUNC_VARIABLE = 20,
          XSLT_FUNC_WHEN = 21,
          XSLT_FUNC_EXTENSION = 22,
          XSLT_FUNC_OTHERWISE = 23,
          XSLT_FUNC_FALLBACK = 24,
          XSLT_FUNC_MESSAGE = 25,
          XSLT_FUNC_INCLUDE = 26,
          XSLT_FUNC_ATTRSET = 27,
          XSLT_FUNC_LITERAL_RESULT_ELEMENT = 28,
          XSLT_FUNC_UNKOWN_FORWARDS_COMPAT = 29);

      xsltTransformState = (
          XSLT_STATE_OK = 0,
          XSLT_STATE_ERROR = 1,
          XSLT_STATE_STOPPED = 2);

       xsltCompMatchPtr = ^xsltCompMatch;
       xsltCompilerCtxtPtr = ^xsltCompilerCtxt;
       xsltCompilerNodeInfoPtr = ^xsltCompilerNodeInfo;
       xsltDecimalFormatPtr = ^xsltDecimalFormat;
       xsltDocumentPtr = ^xsltDocument;
       xsltEffectiveNsPtr = ^xsltEffectiveNs;
       xsltElemPreCompPtr = ^xsltElemPreComp;
       xsltFormatNumberInfoPtr = ^xsltFormatNumberInfo;
       xsltKeyDefPtr = ^xsltKeyDef;
       xsltKeyTablePtr = ^xsltKeyTable;
       xsltNsAliasPtr = ^xsltNsAlias;
       xsltNsListContainerPtr = ^xsltNsListContainer;
       xsltNsListPtr = ^xsltNsList;
       xsltNsMapPtr = ^xsltNsMap;
       xsltNumberDataPtr = ^xsltNumberData;
       xsltPointerListPtr = ^xsltPointerList;
       xsltPrincipalStylesheetDataPtr = ^xsltPrincipalStylesheetData;
       xsltRuntimeExtraPtr = ^xsltRuntimeExtra;
       xsltSecurityPrefsPtr = ^xsltSecurityPrefs;
       xsltStackElemPtr = ^xsltStackElem;
       xsltStyleBasicEmptyItemPtr = ^xsltStyleBasicEmptyItem;
       xsltStyleBasicExpressionItemPtr = ^xsltStyleBasicExpressionItem;
       xsltStyleBasicItemVariablePtr = ^xsltStyleBasicItemVariable;
       xsltStyleItemApplyImportsPtr = ^xsltStyleItemApplyImports;
       xsltStyleItemApplyTemplatesPtr = ^xsltStyleItemApplyTemplates;
       xsltStyleItemAttributePtr = ^xsltStyleItemAttribute;
       xsltStyleItemCallTemplatePtr = ^xsltStyleItemCallTemplate;
       xsltStyleItemChoosePtr = ^xsltStyleItemChoose;
       xsltStyleItemCommentPtr = ^xsltStyleItemComment;
       xsltStyleItemCopyOfPtr = ^xsltStyleItemCopyOf;
       xsltStyleItemCopyPtr = ^xsltStyleItemCopy;
       xsltStyleItemDocumentPtr = ^xsltStyleItemDocument;
       xsltStyleItemElementPtr = ^xsltStyleItemElement;
       xsltStyleItemExtElementPtr = ^xsltStyleItemExtElement;
       xsltStyleItemFallbackPtr = ^xsltStyleItemFallback;
       xsltStyleItemForEachPtr = ^xsltStyleItemForEach;
       xsltStyleItemIfPtr = ^xsltStyleItemIf;
       xsltStyleItemIncludePtr = ^xsltStyleItemInclude;
       xsltStyleItemLRElementInfoPtr = ^xsltStyleItemLRElementInfo;
       xsltStyleItemMessagePtr = ^xsltStyleItemMessage;
       xsltStyleItemNumberPtr = ^xsltStyleItemNumber;
       xsltStyleItemOtherwisePtr = ^xsltStyleItemOtherwise;
       xsltStyleItemPIPtr = ^xsltStyleItemPI;
       xsltStyleItemParamPtr = ^xsltStyleItemParam;
       xsltStyleItemSortPtr = ^xsltStyleItemSort;
       xsltStyleItemTextPtr = ^xsltStyleItemText;
       xsltStyleItemUknownPtr = ^xsltStyleItemUknown;
       xsltStyleItemValueOfPtr = ^xsltStyleItemValueOf;
       xsltStyleItemVariablePtr = ^xsltStyleItemVariable;
       xsltStyleItemWhenPtr = ^xsltStyleItemWhen;
       xsltStyleItemWithParamPtr = ^xsltStyleItemWithParam;
       xsltStylePreCompPtr = ^xsltStylePreComp;
       xsltStylesheetPtr = ^xsltStylesheet;
       xsltTemplatePtr = ^xsltTemplate;
       xsltTransformCachePtr = ^xsltTransformCache;
       xsltTransformContextPtr = ^xsltTransformContext;
       xsltVarInfoPtr = ^xsltVarInfo;
      xsltAddCallCallback = function  (templ: xsltTemplatePtr; source: xmlNodePtr) : Longint; cdecl;
        xsltAddCallCallbackPtr = ^xsltAddCallCallback;

      xsltDocLoaderFunc = function  (const URI: xmlCharPtr; dict: xmlDictPtr; options: Longint; ctxt: Pointer; type_: xsltLoadType) : xmlDocPtr; cdecl;
        xsltDocLoaderFuncPtr = ^xsltDocLoaderFunc;

      xsltDropCallCallback = procedure  (); cdecl;
        xsltDropCallCallbackPtr = ^xsltDropCallCallback;

      xsltElemPreCompDeallocator = procedure  (comp: xsltElemPreCompPtr); cdecl;
        xsltElemPreCompDeallocatorPtr = ^xsltElemPreCompDeallocator;

      xsltExtInitFunction = function  (ctxt: xsltTransformContextPtr; const URI: xmlCharPtr) : Pointer; cdecl;
        xsltExtInitFunctionPtr = ^xsltExtInitFunction;

      xsltExtShutdownFunction = procedure  (ctxt: xsltTransformContextPtr; const URI: xmlCharPtr; data: Pointer); cdecl;
        xsltExtShutdownFunctionPtr = ^xsltExtShutdownFunction;

      xsltHandleDebuggerCallback = procedure  (cur: xmlNodePtr; node: xmlNodePtr; templ: xsltTemplatePtr; ctxt: xsltTransformContextPtr); cdecl;
        xsltHandleDebuggerCallbackPtr = ^xsltHandleDebuggerCallback;

      xsltTransformFunction = procedure  (ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); cdecl;
        xsltTransformFunctionPtr = ^xsltTransformFunction;

      xsltPreComputeFunction = function  (style: xsltStylesheetPtr; inst: xmlNodePtr; function_: xsltTransformFunction) : xsltElemPreCompPtr; cdecl;
        xsltPreComputeFunctionPtr = ^xsltPreComputeFunction;

      xsltSecurityCheck = function  (sec: xsltSecurityPrefsPtr; ctxt: xsltTransformContextPtr; const value: PChar) : Longint; cdecl;
        xsltSecurityCheckPtr = ^xsltSecurityCheck;

      xsltSortFunc = procedure  (ctxt: xsltTransformContextPtr; sorts: xmlNodePtrPtr; nbsorts: Longint); cdecl;
        xsltSortFuncPtr = ^xsltSortFunc;

      xsltStyleExtInitFunction = function  (style: xsltStylesheetPtr; const URI: xmlCharPtr) : Pointer; cdecl;
        xsltStyleExtInitFunctionPtr = ^xsltStyleExtInitFunction;

      xsltStyleExtShutdownFunction = procedure  (style: xsltStylesheetPtr; const URI: xmlCharPtr; data: Pointer); cdecl;
        xsltStyleExtShutdownFunctionPtr = ^xsltStyleExtShutdownFunction;

      xsltTopLevelFunction = procedure  (style: xsltStylesheetPtr; inst: xmlNodePtr); cdecl;
        xsltTopLevelFunctionPtr = ^xsltTopLevelFunction;

      xsltCompMatch = record
      end;

      xsltCompilerCtxt = record
          errorCtxt : Pointer; {* used for error/warning reports; e.g. XSLT_ERROR_SEVERITY_WARNING}
          errSeverity : xsltErrorSeverityType; {}
          warnings : Longint; { TODO: number of warnings found at
compilation}
          errors : Longint; { TODO: number of errors found at
compilation}
          dict : xmlDictPtr; {}
          style : xsltStylesheetPtr; {}
          simplified : Longint; { whether this is a simplified stylesheet TODO: structured/unstructured error contexts.}
          depth : Longint; { Current depth of processing}
          inode : xsltCompilerNodeInfoPtr; {}
          inodeList : xsltCompilerNodeInfoPtr; {}
          inodeLast : xsltCompilerNodeInfoPtr; {}
          tmpList : xsltPointerListPtr; {* The XSLT version as specified by the stylesheet's root element.
*}
          isInclude : Longint; {}
          hasForwardsCompat : Longint; { whether forwards-compatible mode was used
in a parsing episode}
          maxNodeInfos : Longint; { TEMP TODO: just for the interest}
          maxLREs : Longint; {* In order to keep the old behaviour, applying strict rules of
* the spec can be turned off. This has effect only on special
* mechanisms like whitespace-stripping in the stylesheet.
*}
          strict_ : Longint; {}
          psData : xsltPrincipalStylesheetDataPtr; {}
          xpathCtxt : xmlXPathContextPtr; {}
          unknownItem : xsltStyleItemUknownPtr; {}
          hasNsAliases : Longint; { Indicator if there was an xsl:namespace-alias.}
          nsAliases : xsltNsAliasPtr; {}
          ivars : xsltVarInfoPtr; { Storage of local in-scope variables/params.}
          ivar : xsltVarInfoPtr; { topmost local variable/param.}
      end;

      xsltCompilerNodeInfo = record
          next : xsltCompilerNodeInfoPtr; {}
          prev : xsltCompilerNodeInfoPtr; {}
          node : xmlNodePtr; {}
          depth : Longint; {}
          templ : xsltTemplatePtr; { The owning template}
          category : Longint; { XSLT element, LR-element or
extension element}
          type_ : xsltStyleType; {}
          item : xsltElemPreCompPtr; { The compiled information The current in-scope namespaces}
          inScopeNs : xsltNsListContainerPtr; { The current excluded result namespaces}
          exclResultNs : xsltPointerListPtr; { The current extension instruction namespaces}
          extElemNs : xsltPointerListPtr; { The current info for literal result elements.}
          litResElemInfo : xsltStyleItemLRElementInfoPtr; {* Set to 1 if in-scope namespaces changed,
*  or excluded result namespaces changed,
*  or extension element namespaces changed.
* This will trigger creation of new infos
*  for literal result elements.
*}
          nsChanged : Longint; {}
          preserveWhitespace : Longint; {}
          stripWhitespace : Longint; {}
          isRoot : Longint; { whether this is the stylesheet's root node}
          forwardsCompat : Longint; { whether forwards-compatible mode is enabled whether the content of an extension element was processed}
          extContentHandled : Longint; { the type of the current child}
          curChildType : xsltStyleType; {}
      end;

      xsltDecimalFormat = record
          next : xsltDecimalFormatPtr; { chained list}
          name : xmlCharPtr; { Used for interpretation of pattern}
          digit : xmlCharPtr; {}
          patternSeparator : xmlCharPtr; { May appear in result}
          minusSign : xmlCharPtr; {}
          infinity : xmlCharPtr; {}
          noNumber : xmlCharPtr; { Not-a-number Used for interpretation of pattern and may appear in result}
          decimalPoint : xmlCharPtr; {}
          grouping : xmlCharPtr; {}
          percent : xmlCharPtr; {}
          permille : xmlCharPtr; {}
          zeroDigit : xmlCharPtr; {}
      end;

      xsltDocument = record
          next : xsltDocumentPtr; { documents are kept in a chained list}
          main : Longint; { is this the main document}
          doc : xmlDocPtr; { the parsed document}
          keys : Pointer; { key tables storage}
          includes : xsltDocumentPtr; { subsidiary includes}
          preproc : Longint; { pre-processing already done}
          nbKeysComputed : Longint; {}
      end;

      xsltEffectiveNs = record
          nextInStore : xsltEffectiveNsPtr; { storage next}
          next : xsltEffectiveNsPtr; { next item in the list}
          prefix : xmlCharPtr; {}
          nsName : xmlCharPtr; {* Indicates if eclared on the literal result element; dunno if really
* needed.
*}
          holdByElem : Longint; {}
      end;

      xsltElemPreComp = record
          next : xsltElemPreCompPtr; { next item in the global chained
list hold by xsltStylesheet.}
          type_ : xsltStyleType; { type of the element}
          func : xsltTransformFunction; { handling function}
          inst : xmlNodePtr; { the node in the stylesheet's tree
corresponding to this item end of common part}
          free : xsltElemPreCompDeallocator; { the deallocator}
      end;

      xsltFormatNumberInfo = record
          integer_hash : Longint; { Number of '#' in integer part}
          integer_digits : Longint; { Number of '0' in integer part}
          frac_digits : Longint; { Number of '0' in fractional part}
          frac_hash : Longint; { Number of '#' in fractional part}
          group : Longint; { Number of chars per display 'group'}
          multiplier : Longint; { Scaling for percent or permille}
          add_decimal : char; { Flag for whether decimal point appears in pattern}
          is_multiplier_set : char; { Flag to catch multiple occurences of percent/permille}
          is_negative_pattern : char; { Flag for processing -ve prefix/suffix}
      end;

      xsltKeyDef = record
          next : xsltKeyDefPtr; {}
          inst : xmlNodePtr; {}
          name : xmlCharPtr; {}
          nameURI : xmlCharPtr; {}
          match : xmlCharPtr; {}
          use : xmlCharPtr; {}
          comp : xmlXPathCompExprPtr; {}
          usecomp : xmlXPathCompExprPtr; {}
          nsList : xmlNsPtrPtr; { the namespaces in scope}
          nsNr : Longint; { the number of namespaces in scope}
      end;

      xsltKeyTable = record
          next : xsltKeyTablePtr; {}
          name : xmlCharPtr; {}
          nameURI : xmlCharPtr; {}
          keys : xmlHashTablePtr; {}
      end;

      xsltNsAlias = record
          next : xsltNsAliasPtr; { next in the list}
          literalNs : xmlNsPtr; {}
          targetNs : xmlNsPtr; {}
          docOfTargetNs : xmlDocPtr; {}
      end;

      xsltNsList = record
          next : xsltNsListPtr; { next in the list}
          ns : xmlNsPtr; {}
      end;

      xsltNsListContainer = record
          list : xmlNsPtrPtr; {}
          totalNumber : Longint; {}
          xpathNumber : Longint; {}
      end;

      xsltNsMap = record
          next : xsltNsMapPtr; { next in the list}
          doc : xmlDocPtr; {}
          elem : xmlNodePtr; { the element holding the ns-decl}
          ns : xmlNsPtr; { the xmlNs structure holding the XML namespace name}
          origNsName : xmlCharPtr; { the original XML namespace name}
          newNsName : xmlCharPtr; { the mapped XML namespace name}
      end;

      xsltNumberData = record
          level : xmlCharPtr; {}
          count : xmlCharPtr; {}
          from : xmlCharPtr; {}
          value : xmlCharPtr; {}
          format : xmlCharPtr; {}
          has_format : Longint; {}
          digitsPerGroup : Longint; {}
          groupingCharacter : Longint; {}
          groupingCharacterLen : Longint; {}
          doc : xmlDocPtr; {}
          node : xmlNodePtr; {* accelerators
*}
      end;

      xsltPointerList = record
          items : PPointer; {}
          number : Longint; {}
          size : Longint; {}
      end;

      xsltPrincipalStylesheetData = record
          namespaceDict : xmlDictPtr; {* Global list of in-scope namespaces.
*}
          inScopeNamespaces : xsltPointerListPtr; {* Global list of information for [xsl:]excluded-result-prefixes.
*}
          exclResultNamespaces : xsltPointerListPtr; {* Global list of information for [xsl:]extension-element-prefixes.
*}
          extElemNamespaces : xsltPointerListPtr; {}
          effectiveNs : xsltEffectiveNsPtr; {* Namespace name map to get rid of string comparison of namespace names.
*}
          nsMap : xsltNsMapPtr; {}
      end;

      xsltRuntimeExtra = record
          info : Pointer; { pointer to the extra data}
          deallocate : xmlFreeFunc; { pointer to the deallocation routine}
      end;

      xsltSecurityPrefs = record
      end;

      xsltStackElem = record
          next : xsltStackElemPtr; { chained list}
          comp : xsltStylePreCompPtr; { the compiled form}
          computed : Longint; { was the evaluation done}
          name : xmlCharPtr; { the local part of the name QName}
          nameURI : xmlCharPtr; { the URI part of the name QName}
          select : xmlCharPtr; { the eval string}
          tree : xmlNodePtr; { the sequence constructor if no eval
string or the location}
          value : xmlXPathObjectPtr; { The value if computed}
          fragment : xmlDocPtr; { The Result Tree Fragments (needed for XSLT 1.0)
which are bound to the variable's lifetime.}
          level : Longint; { the depth in the tree;
-1 if persistent (e.g. a given xsl:with-param)}
          context : xsltTransformContextPtr; { The transformation context; needed to cache
the variables}
          flags : Longint; {}
      end;

      xsltStyleBasicEmptyItem = record
      end;

      xsltStyleBasicExpressionItem = record
          select : xmlCharPtr; { TODO: Change this to "expression".}
          comp : xmlXPathCompExprPtr; { TODO: Change this to compExpr.}
      end;

      xsltStyleBasicItemVariable = record
          select : xmlCharPtr; {}
          comp : xmlXPathCompExprPtr; {}
          name : xmlCharPtr; {}
          has_name : Longint; {}
          ns : xmlCharPtr; {}
          has_ns : Longint; {}
      end;

      xsltStyleItemApplyTemplates = record
          mode : xmlCharPtr; { apply-templates}
          modeURI : xmlCharPtr; { apply-templates}
          select : xmlCharPtr; { sort, copy-of, value-of, apply-templates}
          comp : xmlXPathCompExprPtr; { a precompiled XPath expression TODO: with-params}
      end;

      xsltStyleItemAttribute = record
          name : xmlCharPtr; {}
          has_name : Longint; {}
          ns : xmlCharPtr; {}
          nsPrefix : xmlCharPtr; {}
          has_ns : Longint; {}
      end;

      xsltStyleItemCallTemplate = record
          templ : xsltTemplatePtr; { call-template}
          name : xmlCharPtr; { element, attribute, pi}
          has_name : Longint; { element, attribute, pi}
          ns : xmlCharPtr; { element}
          has_ns : Longint; { element TODO: with-params}
      end;

      xsltStyleItemCopy = record
          use : xmlCharPtr; { copy, element}
          has_use : Longint; { copy, element}
      end;

      xsltStyleItemDocument = record
          ver11 : Longint; { assigned: in xsltDocumentComp;
read: nowhere;
TODO: Check if we need.}
          filename : xmlCharPtr; { document URL}
          has_filename : Longint; {}
      end;

      xsltStyleItemElement = record
          use : xmlCharPtr; {}
          has_use : Longint; {}
          name : xmlCharPtr; {}
          has_name : Longint; {}
          ns : xmlCharPtr; {}
          nsPrefix : xmlCharPtr; {}
          has_ns : Longint; {}
      end;

      xsltStyleItemExtElement = record
          item : xsltElemPreCompPtr; {}
      end;

      xsltStyleItemIf = record
          test : xmlCharPtr; { if}
          comp : xmlXPathCompExprPtr; { a precompiled XPath expression}
      end;

      xsltStyleItemInclude = record
          include : xsltDocumentPtr; {}
      end;

      xsltStyleItemLRElementInfo = record
          effectiveNs : xsltEffectiveNsPtr; {}
      end;

      xsltStyleItemMessage = record
          terminate : Longint; {}
      end;

      xsltStyleItemNumber = record
          numdata : xsltNumberData; { number}
      end;

      xsltStyleItemOtherwise = record
      end;

      xsltStyleItemPI = record
          name : xmlCharPtr; {}
          has_name : Longint; {}
      end;

      xsltStyleItemParam = record
          select : xmlCharPtr; {}
          comp : xmlXPathCompExprPtr; {}
          name : xmlCharPtr; {}
          has_name : Longint; {}
          ns : xmlCharPtr; {}
          has_ns : Longint; {}
      end;

      xsltStyleItemSort = record
          stype : xmlCharPtr; { sort}
          has_stype : Longint; { sort}
          number : Longint; { sort}
          order : xmlCharPtr; { sort}
          has_order : Longint; { sort}
          descending : Longint; { sort}
          lang : xmlCharPtr; { sort}
          has_lang : Longint; { sort}
          case_order : xmlCharPtr; { sort}
          lower_first : Longint; { sort}
          use : xmlCharPtr; {}
          has_use : Longint; {}
          select : xmlCharPtr; { sort, copy-of, value-of, apply-templates}
          comp : xmlXPathCompExprPtr; { a precompiled XPath expression}
      end;

      xsltStyleItemText = record
          noescape : Longint; { text}
      end;

      xsltStyleItemUknown = record
      end;

      xsltStyleItemValueOf = record
          select : xmlCharPtr; {}
          comp : xmlXPathCompExprPtr; { a precompiled XPath expression}
          noescape : Longint; {}
      end;

      xsltStyleItemWhen = record
          test : xmlCharPtr; {}
          comp : xmlXPathCompExprPtr; {}
      end;

      xsltStylePreComp = record
          next : xsltElemPreCompPtr; { chained list}
          type_ : xsltStyleType; { type of the element}
          func : xsltTransformFunction; { handling function}
          inst : xmlNodePtr; {* Pre computed values.
*}
          stype : xmlCharPtr; { sort}
          has_stype : Longint; { sort}
          number : Longint; { sort}
          order : xmlCharPtr; { sort}
          has_order : Longint; { sort}
          descending : Longint; { sort}
          lang : xmlCharPtr; { sort}
          has_lang : Longint; { sort}
          case_order : xmlCharPtr; { sort}
          lower_first : Longint; { sort}
          use : xmlCharPtr; { copy, element}
          has_use : Longint; { copy, element}
          noescape : Longint; { text}
          name : xmlCharPtr; { element, attribute, pi}
          has_name : Longint; { element, attribute, pi}
          ns : xmlCharPtr; { element}
          has_ns : Longint; { element}
          mode : xmlCharPtr; { apply-templates}
          modeURI : xmlCharPtr; { apply-templates}
          test : xmlCharPtr; { if}
          templ : xsltTemplatePtr; { call-template}
          select : xmlCharPtr; { sort, copy-of, value-of, apply-templates}
          ver11 : Longint; { document}
          filename : xmlCharPtr; { document URL}
          has_filename : Longint; { document}
          numdata : xsltNumberData; { number}
          comp : xmlXPathCompExprPtr; { a precompiled XPath expression}
          nsList : xmlNsPtrPtr; { the namespaces in scope}
          nsNr : Longint; { the number of namespaces in scope}
      end;

      xsltStylesheet = record
          parent : xsltStylesheetPtr; {}
          next : xsltStylesheetPtr; {}
          imports : xsltStylesheetPtr; {}
          docList : xsltDocumentPtr; {* General data on the style sheet document.
*}
          doc : xmlDocPtr; { the parsed XML stylesheet}
          stripSpaces : xmlHashTablePtr; { the hash table of the strip-space and
preserve space elements}
          stripAll : Longint; { strip-space * (1) preserve-space * (-1)}
          cdataSection : xmlHashTablePtr; {* Global variable or parameters.
*}
          variables : xsltStackElemPtr; {* Template descriptions.
*}
          templates : xsltTemplatePtr; { the ordered list of templates}
          templatesHash : Pointer; { hash table or wherever compiled templates
informations are stored}
          rootMatch : Pointer; { template based on /}
          keyMatch : Pointer; { template based on key()}
          elemMatch : Pointer; { template based on *}
          attrMatch : Pointer; { template based on @*}
          parentMatch : Pointer; { template based on ..}
          textMatch : Pointer; { template based on text()}
          piMatch : Pointer; { template based on processing-instruction()}
          commentMatch : Pointer; {* Namespace aliases.
* NOTE: Not used in the refactored code.
*}
          nsAliases : xmlHashTablePtr; {* Attribute sets.
*}
          attributeSets : xmlHashTablePtr; {* Namespaces.
* TODO: Eliminate this.
*}
          nsHash : xmlHashTablePtr; { the set of namespaces in use:
ATTENTION: This is used for
execution of XPath expressions; unfortunately
it restricts the stylesheet to have distinct
prefixes.
TODO: We need to get rid of this.
*}
          nsDefs : Pointer; {* Key definitions.
*}
          keys : Pointer; {* Output related stuff.
*}
          method : xmlCharPtr; { the output method}
          methodURI : xmlCharPtr; { associated namespace if any}
          version : xmlCharPtr; { version string}
          encoding : xmlCharPtr; { encoding string}
          omitXmlDeclaration : Longint; {* Number formatting.
*}
          decimalFormat : xsltDecimalFormatPtr; {}
          standalone : Longint; { standalone = "yes" | "no"}
          doctypePublic : xmlCharPtr; { doctype-public string}
          doctypeSystem : xmlCharPtr; { doctype-system string}
          indent : Longint; { should output being indented}
          mediaType : xmlCharPtr; {* Precomputed blocks.
*}
          preComps : xsltElemPreCompPtr; { list of precomputed blocks}
          warnings : Longint; { number of warnings found at compilation}
          errors : Longint; { number of errors found at compilation}
          exclPrefix : xmlCharPtr; { last excluded prefixes}
          exclPrefixTab : xmlCharPtrPtr; { array of excluded prefixes}
          exclPrefixNr : Longint; { number of excluded prefixes in scope}
          exclPrefixMax : Longint; { size of the array}
          _private : Pointer; {* Extensions.
*}
          extInfos : xmlHashTablePtr; { the extension data}
          extrasNr : Longint; {* For keeping track of nested includes
*}
          includes : xsltDocumentPtr; {* dictionary: shared between stylesheet, context and documents.
*}
          dict : xmlDictPtr; {* precompiled attribute value templates.
*}
          attVTs : Pointer; {* if namespace-alias has an alias for the default stylesheet prefix
* NOTE: Not used in the refactored code.
*}
          defaultAlias : xmlCharPtr; {* bypass pre-processing (already done) (used in imports)
*}
          nopreproc : Longint; {* all document text strings were internalized
*}
          internalized : Longint; {* Literal Result Element as Stylesheet c.f. section 2.3
*}
          literal_result : Longint; {* The principal stylesheet
*}
          principal : xsltStylesheetPtr; {* Compilation context used during compile-time.
*}
          compCtxt : xsltCompilerCtxtPtr; { TODO: Change this to (void *).}
          principalData : xsltPrincipalStylesheetDataPtr; {}
      end;

      xsltTemplate = record
          next : xsltTemplatePtr; { chained list sorted by priority}
          style : xsltStylesheetPtr; { the containing stylesheet}
          match : xmlCharPtr; { the matching string}
          priority : Single; { as given from the stylesheet, not computed}
          name : xmlCharPtr; { the local part of the name QName}
          nameURI : xmlCharPtr; { the URI part of the name QName}
          mode : xmlCharPtr; { the local part of the mode QName}
          modeURI : xmlCharPtr; { the URI part of the mode QName}
          content : xmlNodePtr; { the template replacement value}
          elem : xmlNodePtr; {* TODO: @inheritedNsNr and @inheritedNs won't be used in the
*  refactored code.
*}
          inheritedNsNr : Longint; { number of inherited namespaces}
          inheritedNs : xmlNsPtrPtr; { inherited non-excluded namespaces Profiling informations}
          nbCalls : Longint; { the number of time the template was called}
          time : Cardinal; { the time spent in this template}
          params : Pointer; { xsl:param instructions}
      end;

      xsltTransformCache = record
          RVT : xmlDocPtr; {}
          nbRVT : Longint; {}
          stackItems : xsltStackElemPtr; {}
          nbStackItems : Longint; {}
          dbgCachedRVTs : Longint; {}
          dbgReusedRVTs : Longint; {}
          dbgCachedVars : Longint; {}
          dbgReusedVars : Longint; {}
      end;

      xsltTransformContext = record
          style : xsltStylesheetPtr; { the stylesheet used}
          type_ : xsltOutputType; { the type of output}
          templ : xsltTemplatePtr; { the current template}
          templNr : Longint; { Nb of templates in the stack}
          templMax : Longint; { Size of the templtes stack}
          templTab : xsltTemplatePtrPtr; { the template stack}
          vars : xsltStackElemPtr; { the current variable list}
          varsNr : Longint; { Nb of variable list in the stack}
          varsMax : Longint; { Size of the variable list stack}
          varsTab : xsltStackElemPtrPtr; { the variable list stack}
          varsBase : Longint; {* Extensions
*}
          extFunctions : xmlHashTablePtr; { the extension functions}
          extElements : xmlHashTablePtr; { the extension elements}
          extInfos : xmlHashTablePtr; { the extension data}
          mode : xmlCharPtr; { the current mode}
          modeURI : xmlCharPtr; { the current mode URI}
          docList : xsltDocumentPtr; { the document list}
          document : xsltDocumentPtr; { the current source document; can be NULL if an RTF}
          node : xmlNodePtr; { the current node being processed}
          nodeList : xmlNodeSetPtr; { the current node list xmlNodePtr current;			the node}
          output : xmlDocPtr; { the resulting document}
          insert : xmlNodePtr; { the insertion node}
          xpathCtxt : xmlXPathContextPtr; { the XPath context}
          state : xsltTransformState; {* Global variables
*}
          globalVars : xmlHashTablePtr; { the global variables and params}
          inst : xmlNodePtr; { the instruction in the stylesheet}
          xinclude : Longint; { should XInclude be processed}
          outputFile : PChar; { the output URI if known}
          profile : Longint; { is this run profiled}
          prof : Longint; { the current profiled value}
          profNr : Longint; { Nb of templates in the stack}
          profMax : Longint; { Size of the templtaes stack}
          profTab : PLongInt; { the profile template stack}
          _private : Pointer; { user defined data}
          extrasNr : Longint; { the number of extras used}
          extrasMax : Longint; { the number of extras allocated}
          extras : xsltRuntimeExtraPtr; { extra per runtime informations}
          styleList : xsltDocumentPtr; { the stylesheet docs list}
          sec : Pointer; { the security preferences if any}
          error : xmlGenericErrorFunc; { a specific error handler}
          errctx : Pointer; { context for the error handler}
          sortfunc : xsltSortFunc; {* handling of temporary Result Value Tree
* (XSLT 1.0 term: "Result Tree Fragment")
*}
          tmpRVT : xmlDocPtr; { list of RVT without persistance}
          persistRVT : xmlDocPtr; { list of persistant RVTs}
          ctxtflags : Longint; {* Speed optimization when coalescing text nodes
*}
          lasttext : xmlCharPtr; { last text node content}
          lasttsize : Cardinal; { last text node size}
          lasttuse : Cardinal; {* Per Context Debugging
*}
          debugStatus : Longint; { the context level debug status}
          traceCode : PCardinal; { pointer to the variable holding the mask}
          parserOptions : Longint; {* dictionary: shared between stylesheet, context and documents.
*}
          dict : xmlDictPtr; {}
          tmpDoc : xmlDocPtr; {* all document text strings are internalized
*}
          internalized : Longint; {}
          nbKeys : Longint; {}
          hasTemplKeyPatterns : Longint; {}
          currentTemplateRule : xsltTemplatePtr; { the Current Template Rule}
          initialContextNode : xmlNodePtr; {}
          initialContextDoc : xmlDocPtr; {}
          cache : xsltTransformCachePtr; {}
          contextVariable : Pointer; { the current variable item}
          localRVT : xmlDocPtr; { list of local tree fragments; will be freed when
the instruction which created the fragment
exits}
          localRVTBase : xmlDocPtr; {}
          keyInitLevel : Longint; { Needed to catch recursive keys issues}
      end;

      xsltVarInfo = record
          next : xsltVarInfoPtr; { next in the list}
          prev : xsltVarInfoPtr; {}
          depth : Longint; { the depth in the tree}
          name : xmlCharPtr; {}
          nsName : xmlCharPtr; {}
      end;

      xsltStyleItemApplyImports = xsltStyleBasicEmptyItem;
      xsltStyleItemChoose = xsltStyleBasicEmptyItem;
      xsltStyleItemComment = xsltStyleBasicEmptyItem;
      xsltStyleItemCopyOf = xsltStyleBasicExpressionItem;
      xsltStyleItemFallback = xsltStyleBasicEmptyItem;
      xsltStyleItemForEach = xsltStyleBasicExpressionItem;
      xsltStyleItemVariable = xsltStyleBasicItemVariable;
      xsltStyleItemWithParam = xsltStyleBasicItemVariable;

  function xslAddCall (templ: xsltTemplatePtr; source: xmlNodePtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xslDropCall (); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xslHandleDebugger (cur: xmlNodePtr; node: xmlNodePtr; templ: xsltTemplatePtr; ctxt: xsltTransformContextPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltAddKey (style: xsltStylesheetPtr; const name: xmlCharPtr; const nameURI: xmlCharPtr; const match: xmlCharPtr; const use: xmlCharPtr; inst: xmlNodePtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltAddStackElemList (ctxt: xsltTransformContextPtr; elems: xsltStackElemPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltAddTemplate (style: xsltStylesheetPtr; cur: xsltTemplatePtr; const mode: xmlCharPtr; const modeURI: xmlCharPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltAllocateExtra (style: xsltStylesheetPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltAllocateExtraCtxt (ctxt: xsltTransformContextPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltApplyAttributeSet (ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; const attrSets: xmlCharPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltApplyImports (ctxt: xsltTransformContextPtr; contextNode: xmlNodePtr; inst: xmlNodePtr; comp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltApplyOneTemplate (ctxt: xsltTransformContextPtr; contextNode: xmlNodePtr; list: xmlNodePtr; templ: xsltTemplatePtr; params: xsltStackElemPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltApplyStripSpaces (ctxt: xsltTransformContextPtr; node: xmlNodePtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltApplyStylesheet (style: xsltStylesheetPtr; doc: xmlDocPtr; const params: PPChar) : xmlDocPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltApplyStylesheetUser (style: xsltStylesheetPtr; doc: xmlDocPtr; const params: PPChar; const output: PChar; profile: PFILE; userCtxt: xsltTransformContextPtr) : xmlDocPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltApplyTemplates (ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; castedComp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltAttrListTemplateProcess (ctxt: xsltTransformContextPtr; target: xmlNodePtr; attrs: xmlAttrPtr) : xmlAttrPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltAttrTemplateProcess (ctxt: xsltTransformContextPtr; target: xmlNodePtr; attr: xmlAttrPtr) : xmlAttrPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltAttrTemplateValueProcess (ctxt: xsltTransformContextPtr; const str: xmlCharPtr) : xmlCharPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltAttrTemplateValueProcessNode (ctxt: xsltTransformContextPtr; const str: xmlCharPtr; inst: xmlNodePtr) : xmlCharPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltAttribute (ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltCalibrateAdjust (delta: Longint); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltCallTemplate (ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; castedComp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltCheckExtPrefix (style: xsltStylesheetPtr; const URI: xmlCharPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltCheckExtURI (style: xsltStylesheetPtr; const URI: xmlCharPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltCheckRead (sec: xsltSecurityPrefsPtr; ctxt: xsltTransformContextPtr; const URL: xmlCharPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltCheckWrite (sec: xsltSecurityPrefsPtr; ctxt: xsltTransformContextPtr; const URL: xmlCharPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltChoose (ctxt: xsltTransformContextPtr; contextNode: xmlNodePtr; inst: xmlNodePtr; comp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltCleanupGlobals (); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltCleanupTemplates (style: xsltStylesheetPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltComment (ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltCompileAttr (style: xsltStylesheetPtr; attr: xmlAttrPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltCompilePattern (const pattern: xmlCharPtr; doc: xmlDocPtr; node: xmlNodePtr; style: xsltStylesheetPtr; runtime: xsltTransformContextPtr) : xsltCompMatchPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltComputeSortResult (ctxt: xsltTransformContextPtr; sort: xmlNodePtr) : xmlXPathObjectPtrPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltCopy (ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; castedComp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltCopyNamespace (ctxt: xsltTransformContextPtr; elem: xmlNodePtr; ns: xmlNsPtr) : xmlNsPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltCopyNamespaceList (ctxt: xsltTransformContextPtr; node: xmlNodePtr; cur: xmlNsPtr) : xmlNsPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltCopyOf (ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; castedComp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltCopyTextString (ctxt: xsltTransformContextPtr; target: xmlNodePtr; const string_: xmlCharPtr; noescape: Longint) : xmlNodePtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltCreateRVT (ctxt: xsltTransformContextPtr) : xmlDocPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltDebug (ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltDebugDumpExtensions (output: PFILE); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltDebugGetDefaultTrace () : xsltDebugTraceCodes; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltDebugSetDefaultTrace (val: xsltDebugTraceCodes); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltDecimalFormatGetByName (style: xsltStylesheetPtr; name: xmlCharPtr) : xsltDecimalFormatPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltDefaultSortFunction (ctxt: xsltTransformContextPtr; sorts: xmlNodePtrPtr; nbsorts: Longint); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltDoSortFunction (ctxt: xsltTransformContextPtr; sorts: xmlNodePtrPtr; nbsorts: Longint); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltDocumentComp (style: xsltStylesheetPtr; inst: xmlNodePtr; function_: xsltTransformFunction) : xsltElemPreCompPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltDocumentElem (ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; castedComp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltDocumentFunction (ctxt: xmlXPathParserContextPtr; nargs: Longint); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltDocumentSortFunction (list: xmlNodeSetPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltElement (ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; castedComp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltElementAvailableFunction (ctxt: xmlXPathParserContextPtr; nargs: Longint); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltEvalAVT (ctxt: xsltTransformContextPtr; avt: Pointer; node: xmlNodePtr) : xmlCharPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltEvalAttrValueTemplate (ctxt: xsltTransformContextPtr; inst: xmlNodePtr; const name: xmlCharPtr; const ns: xmlCharPtr) : xmlCharPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltEvalGlobalVariables (ctxt: xsltTransformContextPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltEvalOneUserParam (ctxt: xsltTransformContextPtr; const name: xmlCharPtr; const value: xmlCharPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltEvalStaticAttrValueTemplate (style: xsltStylesheetPtr; inst: xmlNodePtr; const name: xmlCharPtr; const ns: xmlCharPtr; found: PInteger) : xmlCharPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltEvalTemplateString (ctxt: xsltTransformContextPtr; contextNode: xmlNodePtr; inst: xmlNodePtr) : xmlCharPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltEvalUserParams (ctxt: xsltTransformContextPtr; const params: PPChar) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltEvalXPathPredicate (ctxt: xsltTransformContextPtr; comp: xmlXPathCompExprPtr; nsList: xmlNsPtrPtr; nsNr: Longint) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltEvalXPathString (ctxt: xsltTransformContextPtr; comp: xmlXPathCompExprPtr) : xmlCharPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltEvalXPathStringNs (ctxt: xsltTransformContextPtr; comp: xmlXPathCompExprPtr; nsNr: Longint; nsList: xmlNsPtrPtr) : xmlCharPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltExtElementLookup (ctxt: xsltTransformContextPtr; const name: xmlCharPtr; const URI: xmlCharPtr) : xsltTransformFunction; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltExtFunctionLookup (ctxt: xsltTransformContextPtr; const name: xmlCharPtr; const URI: xmlCharPtr) : xmlXPathFunction; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltExtModuleElementLookup (const name: xmlCharPtr; const URI: xmlCharPtr) : xsltTransformFunction; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltExtModuleElementPreComputeLookup (const name: xmlCharPtr; const URI: xmlCharPtr) : xsltPreComputeFunction; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltExtModuleFunctionLookup (const name: xmlCharPtr; const URI: xmlCharPtr) : xmlXPathFunction; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltExtModuleTopLevelLookup (const name: xmlCharPtr; const URI: xmlCharPtr) : xsltTopLevelFunction; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltExtensionInstructionResultFinalize (ctxt: xsltTransformContextPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltExtensionInstructionResultRegister (ctxt: xsltTransformContextPtr; obj: xmlXPathObjectPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltFindDocument (ctxt: xsltTransformContextPtr; doc: xmlDocPtr) : xsltDocumentPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltFindElemSpaceHandling (ctxt: xsltTransformContextPtr; node: xmlNodePtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltFindTemplate (ctxt: xsltTransformContextPtr; const name: xmlCharPtr; const nameURI: xmlCharPtr) : xsltTemplatePtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltForEach (ctxt: xsltTransformContextPtr; contextNode: xmlNodePtr; inst: xmlNodePtr; castedComp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltFormatNumberConversion (self: xsltDecimalFormatPtr; format: xmlCharPtr; number: double; result: xmlCharPtrPtr) : xmlXPathErrorEnum; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltFormatNumberFunction (ctxt: xmlXPathParserContextPtr; nargs: Longint); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltFreeAVTList (avt: Pointer); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltFreeAttributeSetsHashes (style: xsltStylesheetPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltFreeCompMatchList (comp: xsltCompMatchPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltFreeCtxtExts (ctxt: xsltTransformContextPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltFreeDocumentKeys (idoc: xsltDocumentPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltFreeDocuments (ctxt: xsltTransformContextPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltFreeExts (style: xsltStylesheetPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltFreeGlobalVariables (ctxt: xsltTransformContextPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltFreeKeys (style: xsltStylesheetPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltFreeNamespaceAliasHashes (style: xsltStylesheetPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltFreeRVTs (ctxt: xsltTransformContextPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltFreeSecurityPrefs (sec: xsltSecurityPrefsPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltFreeStackElemList (elem: xsltStackElemPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltFreeStyleDocuments (style: xsltStylesheetPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltFreeStylePreComps (style: xsltStylesheetPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltFreeStylesheet (style: xsltStylesheetPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltFreeTemplateHashes (style: xsltStylesheetPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltFreeTransformContext (ctxt: xsltTransformContextPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltFunctionAvailableFunction (ctxt: xmlXPathParserContextPtr; nargs: Longint); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltFunctionNodeSet (ctxt: xmlXPathParserContextPtr; nargs: Longint); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltGenerateIdFunction (ctxt: xmlXPathParserContextPtr; nargs: Longint); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltGetCNsProp (style: xsltStylesheetPtr; node: xmlNodePtr; const name: xmlCharPtr; const nameSpace: xmlCharPtr) : xmlCharPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltGetDebuggerStatus () : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltGetDefaultSecurityPrefs () : xsltSecurityPrefsPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltGetExtData (ctxt: xsltTransformContextPtr; const URI: xmlCharPtr) : Pointer; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltGetExtInfo (style: xsltStylesheetPtr; const URI: xmlCharPtr) : xmlHashTablePtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltGetKey (ctxt: xsltTransformContextPtr; const name: xmlCharPtr; const nameURI: xmlCharPtr; const value: xmlCharPtr) : xmlNodeSetPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltGetNamespace (ctxt: xsltTransformContextPtr; cur: xmlNodePtr; ns: xmlNsPtr; out_: xmlNodePtr) : xmlNsPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltGetNsProp (node: xmlNodePtr; const name: xmlCharPtr; const nameSpace: xmlCharPtr) : xmlCharPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltGetPlainNamespace (ctxt: xsltTransformContextPtr; cur: xmlNodePtr; ns: xmlNsPtr; out_: xmlNodePtr) : xmlNsPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltGetProfileInformation (ctxt: xsltTransformContextPtr) : xmlDocPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltGetQNameURI (node: xmlNodePtr; name: xmlCharPtrPtr) : xmlCharPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltGetQNameURI2 (style: xsltStylesheetPtr; node: xmlNodePtr; const name: xmlCharPtrPtr) : xmlCharPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltGetSecurityPrefs (sec: xsltSecurityPrefsPtr; option: xsltSecurityOption) : xsltSecurityCheck; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltGetSpecialNamespace (ctxt: xsltTransformContextPtr; invocNode: xmlNodePtr; const nsName: xmlCharPtr; const nsPrefix: xmlCharPtr; target: xmlNodePtr) : xmlNsPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltGetTemplate (ctxt: xsltTransformContextPtr; node: xmlNodePtr; style: xsltStylesheetPtr) : xsltTemplatePtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltGetUTF8Char (const utf: PByte; len: PInteger) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltGetXIncludeDefault () : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltIf (ctxt: xsltTransformContextPtr; contextNode: xmlNodePtr; inst: xmlNodePtr; castedComp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltInit (); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltInitAllDocKeys (ctxt: xsltTransformContextPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltInitCtxtExts (ctxt: xsltTransformContextPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltInitCtxtKey (ctxt: xsltTransformContextPtr; idoc: xsltDocumentPtr; keyDef: xsltKeyDefPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltInitCtxtKeys (ctxt: xsltTransformContextPtr; idoc: xsltDocumentPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltInitElemPreComp (comp: xsltElemPreCompPtr; style: xsltStylesheetPtr; inst: xmlNodePtr; function_: xsltTransformFunction; freeFunc: xsltElemPreCompDeallocator); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltIsBlank (str: xmlCharPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltKeyFunction (ctxt: xmlXPathParserContextPtr; nargs: Longint); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltLoadDocument (ctxt: xsltTransformContextPtr; const URI: xmlCharPtr) : xsltDocumentPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltLoadStyleDocument (style: xsltStylesheetPtr; const URI: xmlCharPtr) : xsltDocumentPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltLoadStylesheetPI (doc: xmlDocPtr) : xsltStylesheetPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltLocalVariablePop (ctxt: xsltTransformContextPtr; limitNr: Longint; level: Longint); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltLocalVariablePush (ctxt: xsltTransformContextPtr; variable: xsltStackElemPtr; level: Longint) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltMatchPattern (ctxt: xsltTransformContextPtr; node: xmlNodePtr; const pattern: xmlCharPtr; ctxtdoc: xmlDocPtr; ctxtnode: xmlNodePtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltMessage (ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltNamespaceAlias (style: xsltStylesheetPtr; node: xmlNodePtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltNeedElemSpaceHandling (ctxt: xsltTransformContextPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltNewDocument (ctxt: xsltTransformContextPtr; doc: xmlDocPtr) : xsltDocumentPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltNewElemPreComp (style: xsltStylesheetPtr; inst: xmlNodePtr; function_: xsltTransformFunction) : xsltElemPreCompPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltNewSecurityPrefs () : xsltSecurityPrefsPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltNewStyleDocument (style: xsltStylesheetPtr; doc: xmlDocPtr) : xsltDocumentPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltNewStylesheet () : xsltStylesheetPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltNewTransformContext (style: xsltStylesheetPtr; doc: xmlDocPtr) : xsltTransformContextPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltNextImport (cur: xsltStylesheetPtr) : xsltStylesheetPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltNormalizeCompSteps (payload: Pointer; data: Pointer; const name: xmlCharPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltNumber (ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; castedComp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltNumberFormat (ctxt: xsltTransformContextPtr; data: xsltNumberDataPtr; node: xmlNodePtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltParseAnyXSLTElem (cctxt: xsltCompilerCtxtPtr; elem: xmlNodePtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltParseGlobalParam (style: xsltStylesheetPtr; cur: xmlNodePtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltParseGlobalVariable (style: xsltStylesheetPtr; cur: xmlNodePtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltParseSequenceConstructor (cctxt: xsltCompilerCtxtPtr; cur: xmlNodePtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltParseStylesheetAttributeSet (style: xsltStylesheetPtr; cur: xmlNodePtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltParseStylesheetCallerParam (ctxt: xsltTransformContextPtr; inst: xmlNodePtr) : xsltStackElemPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltParseStylesheetDoc (doc: xmlDocPtr) : xsltStylesheetPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltParseStylesheetFile (const filename: xmlCharPtr) : xsltStylesheetPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltParseStylesheetImport (style: xsltStylesheetPtr; cur: xmlNodePtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltParseStylesheetImportedDoc (doc: xmlDocPtr; parentStyle: xsltStylesheetPtr) : xsltStylesheetPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltParseStylesheetInclude (style: xsltStylesheetPtr; cur: xmlNodePtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltParseStylesheetOutput (style: xsltStylesheetPtr; cur: xmlNodePtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltParseStylesheetParam (ctxt: xsltTransformContextPtr; cur: xmlNodePtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltParseStylesheetProcess (ret: xsltStylesheetPtr; doc: xmlDocPtr) : xsltStylesheetPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltParseStylesheetVariable (ctxt: xsltTransformContextPtr; inst: xmlNodePtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltParseTemplateContent (style: xsltStylesheetPtr; templ: xmlNodePtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltPointerListAddSize (list: xsltPointerListPtr; item: Pointer; initialSize: Longint) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltPointerListClear (list: xsltPointerListPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltPointerListCreate (initialSize: Longint) : xsltPointerListPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltPointerListFree (list: xsltPointerListPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltPreComputeExtModuleElement (style: xsltStylesheetPtr; inst: xmlNodePtr) : xsltElemPreCompPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltPrintErrorContext (ctxt: xsltTransformContextPtr; style: xsltStylesheetPtr; node: xmlNodePtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltProcessingInstruction (ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; castedComp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltProfileStylesheet (style: xsltStylesheetPtr; doc: xmlDocPtr; const params: PPChar; output: PFILE) : xmlDocPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltQuoteOneUserParam (ctxt: xsltTransformContextPtr; const name: xmlCharPtr; const value: xmlCharPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltQuoteUserParams (ctxt: xsltTransformContextPtr; const params: PPChar) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltRegisterAllElement (ctxt: xsltTransformContextPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltRegisterAllExtras (); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltRegisterAllFunctions (ctxt: xmlXPathContextPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltRegisterExtElement (ctxt: xsltTransformContextPtr; const name: xmlCharPtr; const URI: xmlCharPtr; function_: xsltTransformFunction) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltRegisterExtFunction (ctxt: xsltTransformContextPtr; const name: xmlCharPtr; const URI: xmlCharPtr; function_: xmlXPathFunction) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltRegisterExtModule (const URI: xmlCharPtr; initFunc: xsltExtInitFunction; shutdownFunc: xsltExtShutdownFunction) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltRegisterExtModuleElement (const name: xmlCharPtr; const URI: xmlCharPtr; precomp: xsltPreComputeFunction; transform: xsltTransformFunction) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltRegisterExtModuleFull (const URI: xmlCharPtr; initFunc: xsltExtInitFunction; shutdownFunc: xsltExtShutdownFunction; styleInitFunc: xsltStyleExtInitFunction; styleShutdownFunc: xsltStyleExtShutdownFunction) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltRegisterExtModuleFunction (const name: xmlCharPtr; const URI: xmlCharPtr; function_: xmlXPathFunction) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltRegisterExtModuleTopLevel (const name: xmlCharPtr; const URI: xmlCharPtr; function_: xsltTopLevelFunction) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltRegisterExtPrefix (style: xsltStylesheetPtr; const prefix: xmlCharPtr; const URI: xmlCharPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltRegisterExtras (ctxt: xsltTransformContextPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltRegisterLocalRVT (ctxt: xsltTransformContextPtr; RVT: xmlDocPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltRegisterPersistRVT (ctxt: xsltTransformContextPtr; RVT: xmlDocPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltRegisterTestModule (); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltRegisterTmpRVT (ctxt: xsltTransformContextPtr; RVT: xmlDocPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltReleaseRVT (ctxt: xsltTransformContextPtr; RVT: xmlDocPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltResolveStylesheetAttributeSet (style: xsltStylesheetPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltRestoreDocumentNamespaces (ns: xsltNsMapPtr; doc: xmlDocPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltRunStylesheet (style: xsltStylesheetPtr; doc: xmlDocPtr; const params: PPChar; const output: PChar; SAX: xmlSAXHandlerPtr; IObuf: xmlOutputBufferPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltRunStylesheetUser (style: xsltStylesheetPtr; doc: xmlDocPtr; const params: PPChar; const output: PChar; SAX: xmlSAXHandlerPtr; IObuf: xmlOutputBufferPtr; profile: PFILE; userCtxt: xsltTransformContextPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltSaveProfiling (ctxt: xsltTransformContextPtr; output: PFILE); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltSaveResultTo (buf: xmlOutputBufferPtr; result: xmlDocPtr; style: xsltStylesheetPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltSaveResultToFd (fd: Longint; result: xmlDocPtr; style: xsltStylesheetPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltSaveResultToFile (file_: PFILE; result: xmlDocPtr; style: xsltStylesheetPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltSaveResultToFilename (const URL: PChar; result: xmlDocPtr; style: xsltStylesheetPtr; compression: Longint) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltSaveResultToString (doc_txt_ptr: xmlCharPtrPtr; doc_txt_len: PInteger; result: xmlDocPtr; style: xsltStylesheetPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltSecurityAllow (sec: xsltSecurityPrefsPtr; ctxt: xsltTransformContextPtr; const value: PChar) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltSecurityForbid (sec: xsltSecurityPrefsPtr; ctxt: xsltTransformContextPtr; const value: PChar) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltSetCtxtParseOptions (ctxt: xsltTransformContextPtr; options: Longint) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltSetCtxtSecurityPrefs (sec: xsltSecurityPrefsPtr; ctxt: xsltTransformContextPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltSetCtxtSortFunc (ctxt: xsltTransformContextPtr; handler: xsltSortFunc); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltSetDebuggerCallbacks (no: Longint; block: Pointer) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltSetDebuggerStatus (value: Longint); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltSetDefaultSecurityPrefs (sec: xsltSecurityPrefsPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltSetGenericDebugFunc (ctx: Pointer; handler: xmlGenericErrorFunc); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltSetGenericErrorFunc (ctx: Pointer; handler: xmlGenericErrorFunc); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltSetLoaderFunc (f: xsltDocLoaderFunc); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltSetSecurityPrefs (sec: xsltSecurityPrefsPtr; option: xsltSecurityOption; func: xsltSecurityCheck) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltSetSortFunc (handler: xsltSortFunc); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltSetTransformErrorFunc (ctxt: xsltTransformContextPtr; ctx: Pointer; handler: xmlGenericErrorFunc); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltSetXIncludeDefault (xinclude: Longint); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltShutdownCtxtExts (ctxt: xsltTransformContextPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltShutdownExts (style: xsltStylesheetPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltSort (ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltSplitQName (dict: xmlDictPtr; const name: xmlCharPtr; const prefix: xmlCharPtrPtr) : xmlCharPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltStyleGetExtData (style: xsltStylesheetPtr; const URI: xmlCharPtr) : Pointer; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltStylePreCompute (style: xsltStylesheetPtr; inst: xmlNodePtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltStyleStylesheetLevelGetExtData (style: xsltStylesheetPtr; const URI: xmlCharPtr) : Pointer; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltSystemPropertyFunction (ctxt: xmlXPathParserContextPtr; nargs: Longint); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltTemplateProcess (ctxt: xsltTransformContextPtr; node: xmlNodePtr) : xmlNodePtrPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltTestCompMatchList (ctxt: xsltTransformContextPtr; node: xmlNodePtr; comp: xsltCompMatchPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltText (ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltTimestamp () : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltTransStorageAdd (ctxt: xsltTransformContextPtr; id: Pointer; data: Pointer) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltTransStorageRemove (ctxt: xsltTransformContextPtr; id: Pointer) : Pointer; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltTransformError (ctxt: xsltTransformContextPtr; style: xsltStylesheetPtr; node: xmlNodePtr; const msg: PChar); cdecl; varargs; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltUninit (); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltUnparsedEntityURIFunction (ctxt: xmlXPathParserContextPtr; nargs: Longint); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltUnregisterExtModule (const URI: xmlCharPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltUnregisterExtModuleElement (const name: xmlCharPtr; const URI: xmlCharPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltUnregisterExtModuleFunction (const name: xmlCharPtr; const URI: xmlCharPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltUnregisterExtModuleTopLevel (const name: xmlCharPtr; const URI: xmlCharPtr) : Longint; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  procedure xsltValueOf (ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; castedComp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltVariableLookup (ctxt: xsltTransformContextPtr; const name: xmlCharPtr; const ns_uri: xmlCharPtr) : xmlXPathObjectPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltXPathCompile (style: xsltStylesheetPtr; const str: xmlCharPtr) : xmlXPathCompExprPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltXPathFunctionLookup (ctxt: xmlXPathContextPtr; const name: xmlCharPtr; const ns_uri: xmlCharPtr) : xmlXPathFunction; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltXPathGetTransformContext (ctxt: xmlXPathParserContextPtr) : xsltTransformContextPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
  function xsltXPathVariableLookup (ctxt: Pointer; const name: xmlCharPtr; const ns_uri: xmlCharPtr) : xmlXPathObjectPtr; cdecl; external LIBXSLT_SO {$IFDEF USE_DELAYED}delayed{$ENDIF};
var
  __xslDebugStatus: PInteger;
var
  __xsltDocDefaultLoader: xsltDocLoaderFuncPtr;
  function xsltEngineVersion(): PChar; cdecl;
var
  __xsltGenericDebug: xmlGenericErrorFuncPtr;
var
  __xsltGenericDebugContext: PPointer;
var
  __xsltGenericError: xmlGenericErrorFuncPtr;
var
  __xsltGenericErrorContext: PPointer;
  function xsltLibxmlVersion(): Longint; cdecl;
  function xsltLibxsltVersion(): Longint; cdecl;
var
  __xsltMaxDepth: PInteger;

const
  xsltConstNamespaceNameXSLT = 'http://www.w3.org/1999/XSL/Transform';
  xsltExtMarker = 'Extension Element';
  xsltConstXSLTAttrMarker = 'LRE XLST Attr';
implementation
uses
{$IFDEF FPC}
   DynLibs,
{$ELSE}
  {$IFDEF MSWINDOWS}
    Windows,
  {$ENDIF}
{$ENDIF}
  SysUtils;

var
  libHandle: TLibHandle;

// Utility function to make sure procedure entry points are not null

procedure CheckForNil(ptr: Pointer; name:string);
begin
  if not Assigned(ptr) then
    raise Exception.Create('"' + name + '" could not be loaded from the dynamic library ' + LIBXSLT_SO);
end;

var
   pxsltEngineVersion: PPChar;

function xsltEngineVersion: PChar; cdecl;
begin
  CheckForNil(pxsltEngineVersion, 'xsltEngineVersion');
  Result := pxsltEngineVersion^;
end;

var
   pxsltLibxmlVersion: PInteger;

function xsltLibxmlVersion: Longint; cdecl;
begin
  CheckForNil(pxsltLibxmlVersion, 'xsltLibxmlVersion');
  Result := pxsltLibxmlVersion^;
end;

var
   pxsltLibxsltVersion: PInteger;

function xsltLibxsltVersion: Longint; cdecl;
begin
  CheckForNil(pxsltLibxsltVersion, 'xsltLibxsltVersion');
  Result := pxsltLibxsltVersion^;
end;

procedure Init;
begin
  // The Delphi 'external' directive can be used for functions and procedures,
  // but here we need to obtain the addresses of POINTERS to functions. We can
  // get to these addresses (and also those of other data values exported from
  // the DLL) by using GetProcAddress.
  libHandle := LoadLibrary(LIBXSLT_SO);
  if libHandle <> 0 then
  begin
    __xslDebugStatus := PInteger(GetProcAddress(libHandle, 'xslDebugStatus'));
    __xsltDocDefaultLoader := xsltDocLoaderFuncPtr(GetProcAddress(libHandle, 'xsltDocDefaultLoader'));
    pxsltEngineVersion := PPChar(GetProcAddress(libHandle, 'xsltEngineVersion'));
    __xsltGenericDebug := xmlGenericErrorFuncPtr(GetProcAddress(libHandle, 'xsltGenericDebug'));
    __xsltGenericDebugContext := PPointer(GetProcAddress(libHandle, 'xsltGenericDebugContext'));
    __xsltGenericError := xmlGenericErrorFuncPtr(GetProcAddress(libHandle, 'xsltGenericError'));
    __xsltGenericErrorContext := PPointer(GetProcAddress(libHandle, 'xsltGenericErrorContext'));
    pxsltLibxmlVersion := PInteger(GetProcAddress(libHandle, 'xsltLibxmlVersion'));
    pxsltLibxsltVersion := PInteger(GetProcAddress(libHandle, 'xsltLibxsltVersion'));
    __xsltMaxDepth := PInteger(GetProcAddress(libHandle, 'xsltMaxDepth'));

    //FreeLibrary(libHandle);
  end;
end;

initialization
  libHandle := 0;

finalization
  if libHandle <> 0 then
    FreeLibrary(libHandle);

end.
