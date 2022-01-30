#include <iostream>
#include <string>
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include <cctype>


#include "antlr4-runtime.h"
#include "grammar426Lexer.h"
#include "grammar426Parser.h"
#include "grammar426BaseVisitor.h"

#include "llvm/ADT/StringMap.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/raw_os_ostream.h"

#ifndef NDEBUG
#define DEBUG(x) do { x << std::endl;} while (0);
#else
#define DEBUG(x)
#endif
/*
using namespace llvm;
using namespace llvm::orc;

class PrototypeAST;

static LLVMContext theLLVMContext;
static IRBuilder<> builder(theLLVMContext);
static std::unique_ptr<Module> module;
static std::map<std::string,Value *> globalValues;
static std::map<std::string,AllocaInst *> namedValues;
static std::unique_ptr<legacy::FunctionPassManager> fpm;
static std::unique_ptr<KaleidoscopeJIT> jit;
static std::map<std::string, std::unique_ptr<PrototypeAST>> functionProtos;
static enum CtxType{
    GLOBAL = 0,
    FUNCTION,
    BLOCK
};
static CtxType curCtx = GLOBAL;
static int anonFnNum = 0;


class PrototypeAST {
    std::string name;
    std::vector<std::string> args;
public:
    PrototypeAST(const std::string &name,std::vector<std::string> args,bool isOperator = false, unsigned prec = 0):name(name),args(std::move(args)){}
    const std::string &getName() const {return name;}
    Function *codeGen();

};

Function *PrototypeAST::codeGen() {
    std::vector<Type *> doubles(args.size(),Type::getDoubleTy(theLLVMContext));
    FunctionType *ft = FunctionType::get(Type::getDoubleTy(theLLVMContext),doubles, false);
    Function *f = Function::Create(ft,Function::ExternalLinkage,name,module.get());
    unsigned idx = 0;
    for(auto &arg : f->args())
        arg.setName(args[idx++]);
    return f;
}

static AllocaInst *CreateEntryBlockAlloca(Function *theFunction,const std::string &varName) {
    IRBuilder<> tmpB(&theFunction->getEntryBlock(),theFunction->getEntryBlock().begin());
    return tmpB.CreateAlloca(Type::getDoubleTy(theLLVMContext),0,varName.c_str());
}
Function *getFunction(std::string name){
    if(auto *f = module->getFunction(name))
        return f;
    auto fi = functionProtos.find(name);
    if(fi!=functionProtos.end())
        return fi->second->codeGen();
    return nullptr;
}

static void initializeModuleAndpassManager(void) {
    module = llvm::make_unique<Module>("llvmlan",theLLVMContext);
    module->setDataLayout(jit->getTargetMachine().createDataLayout());

    fpm = llvm::make_unique<legacy::FunctionPassManager>(module.get());
    fpm->add(createDemoteRegisterToMemoryPass());
    fpm->add(createInstructionCombiningPass());
    fpm->add(createReassociatePass());
    fpm->add(createGVNPass());
    fpm->add(createCFGSimplificationPass());
    fpm->doInitialization();
}

class TreeShapeListener : public WenyanBaseListener {
public:
    void enterProgram(WenyanParser::ProgramContext *context) override {
        WenyanBaseListener::enterProgram(context);
        InitializeNativeTarget();
        InitializeNativeTargetAsmPrinter();
        InitializeNativeTargetAsmParser();
        jit = llvm::make_unique<KaleidoscopeJIT>();
        initializeModuleAndpassManager();
    }

    void exitBlock(WenyanParser::BlockContext *context) override {
        WenyanBaseListener::exitBlock(context);
        context->value = context->statement().back()->value;
    }

    void enterStatement(WenyanParser::StatementContext *context) override {
        WenyanBaseListener::enterStatement(context);
        if(curCtx == GLOBAL){
            std::string fnName = std::to_string(anonFnNum++);
            auto proto = llvm::make_unique<PrototypeAST>(fnName,std::vector<std::string>());
            functionProtos[fnName] = std::move(proto);
            context->theFunction = getFunction(fnName);
            if(!context->theFunction)
                return;
            BasicBlock *bb = BasicBlock::Create(theLLVMContext,"entry",context->theFunction);
            builder.SetInsertPoint(bb);
            curCtx = FUNCTION;
        }
    }

    void exitStatement(WenyanParser::StatementContext *context) override {
        WenyanBaseListener::enterStatement(context);
        if(context->declareNumber()){
            context->value = context->declareNumber()->value;
        }else if(context->assignStatement()){
            context->value = context->assignStatement()->value;
        } else if(context->expression()){
            context->value = context->expression()->value;
        } else {
            context->value = ConstantFP::get(Type::getDoubleTy(theLLVMContext),0.0);
        }
        if(getFunction(std::to_string(anonFnNum-1))){
            if(Value *retVal = context->value){
                builder.CreateRet(retVal);
                verifyFunction(*context->theFunction);
                fpm->run(*context->theFunction);
                curCtx = GLOBAL;
            } else {
                builder.CreateRet(ConstantFP::get(Type::getDoubleTy(theLLVMContext),0.0));
                verifyFunction(*context->theFunction);
                fpm->run(*context->theFunction);
                curCtx = GLOBAL;
            }
            auto h = jit->addModule(std::move(module));
            initializeModuleAndpassManager();
            auto exprSymbol = jit->findSymbol(std::to_string(anonFnNum-1));
            assert(exprSymbol && "Function not found");
            double (*fp)() = (decltype(fp))cantFail(exprSymbol.getAddress());
            fprintf(stderr,"Evaluated to %f\n",fp());

            jit->removeModule(h);
        }
    }

    void exitExpression(WenyanParser::ExpressionContext *context) override {
        WenyanBaseListener::enterExpression(context);
        if(context->YI3()){
            if(context->OP()[0]->getText() == "加"){
                Value *lv = context->fn?context->fn->value:context->fv->value;
                Value *rv = context->sn?context->sn->value:context->sv->value;
                context->value = builder.CreateFAdd(lv,rv,"addtmp");
            } else if(context->OP()[0]->getText() == "减"){
                Value *lv = context->fn?context->fn->value:context->fv->value;
                Value *rv = context->sn?context->sn->value:context->sv->value;
                context->value = builder.CreateFSub(lv,rv,"subtmp");
            } else if(context->OP()[0]->getText() == "乘"){
                Value *lv = context->fn?context->fn->value:context->fv->value;
                Value *rv = context->sn?context->sn->value:context->sv->value;
                context->value = builder.CreateFMul(lv,rv,"multmp");
            }
        } else if (context->OP().size()) {
            Value *lv = context->fn?context->fn->value:context->fv->value;
            Value *rv = context->sn?context->sn->value:context->sv->value;
            if(context->OP()[0]->getText() == "小于"|| context->OP()[0]->getText() == "小於"){
                lv = builder.CreateFCmpULT(lv,rv,"cmptmp");
            } else if (context->OP()[0]->getText() == "大于"|| context->OP()[0]->getText() == "大於"){
                lv = builder.CreateFCmpUGT(lv,rv,"cmptmp");
            } else if (context->OP()[0]->getText() == "等于"|| context->OP()[0]->getText() == "等於"){
                lv = builder.CreateFCmpUEQ(lv,rv,"cmptmp");
            }
            context->value =  builder.CreateUIToFP(lv,Type::getDoubleTy(theLLVMContext),"booltmp");
        }
        else if(context->applyStatement()){
            context->value = context->applyStatement()->applyFunction()->value;
        } else {
            context->value = context->fn?context->fn->value:context->fv->value;
        }
    }

    void enterIfStatement(WenyanParser::IfStatementContext *context) override {
        WenyanBaseListener::enterIfStatement(context);

        Function *theFunction = builder.GetInsertBlock()->getParent();
        context->thenBB = BasicBlock::Create(theLLVMContext,"then",theFunction);
        context->elseBB = BasicBlock::Create(theLLVMContext,"else");
        context->mergeBB = BasicBlock::Create(theLLVMContext,"ifcont");
        WenyanParser::IfStartStateContext *startCtx = context->getRuleContext<WenyanParser::IfStartStateContext>(0);
        startCtx->thenBB = context->thenBB;
        startCtx->elseBB = context->elseBB;
        startCtx->mergeBB = context->mergeBB;
        WenyanParser::IfThenStateContext *thenCtx = context->getRuleContext<WenyanParser::IfThenStateContext>(0);
        thenCtx->thenBB = context->thenBB;
        thenCtx->elseBB = context->elseBB;
        thenCtx->mergeBB = context->mergeBB;
        WenyanParser::IfElseStateContext *elseCtx = context->getRuleContext<WenyanParser::IfElseStateContext>(0);
        elseCtx->thenBB = context->thenBB;
        elseCtx->elseBB = context->elseBB;
        elseCtx->mergeBB = context->mergeBB;

    }

    void exitIfStartState(WenyanParser::IfStartStateContext *context) override {
        WenyanBaseListener::exitIfStartState(context);
        Value *condV = context->expression()->value;
        if(!condV)
            return;
        condV = builder.CreateFCmpONE(condV,ConstantFP::get(theLLVMContext,APFloat(0.0)),"ifcond");
        builder.CreateCondBr(condV,context->thenBB,context->elseBB);
        builder.SetInsertPoint(context->thenBB);
    }

    void exitIfThenState(WenyanParser::IfThenStateContext *context) override {
        Function *theFunction = builder.GetInsertBlock()->getParent();
        if(!context->block()->statement().back()->returnStatement()){
            builder.CreateBr(context->mergeBB);
        }
        theFunction->getBasicBlockList().push_back(context->elseBB);
        builder.SetInsertPoint(context->elseBB);
        context->value = context->block()->value;
    }

    void exitIfElseState(WenyanParser::IfElseStateContext *context) override {
        Function *theFunction = builder.GetInsertBlock()->getParent();
        if(!context->block()->statement().back()->returnStatement()){
            builder.CreateBr(context->mergeBB);
        }
        theFunction->getBasicBlockList().push_back(context->mergeBB);
        builder.SetInsertPoint(context->mergeBB);
        context->value = context->block()->value;
    }

    void exitIfStatement(WenyanParser::IfStatementContext *context) override {
        PHINode *pn = builder.CreatePHI(Type::getDoubleTy(theLLVMContext),2,"iftmp");
        pn->addIncoming(context->ifThenState()->value,context->thenBB);
        pn->addIncoming(context->ifElseState()->value,context->elseBB);
    }

    void enterForStatement(WenyanParser::ForStatementContext *context) override {
        WenyanBaseListener::enterForStatement(context);
        context->theFunction = builder.GetInsertBlock()->getParent();
        context->alloca = CreateEntryBlockAlloca(context->theFunction,"index");
        builder.CreateStore(ConstantFP::get(Type::getDoubleTy(theLLVMContext),0.0),context->alloca);
        context->loopBB = BasicBlock::Create(theLLVMContext,"loop",context->theFunction);
        builder.CreateBr(context->loopBB);
        builder.SetInsertPoint(context->loopBB);
    }

    void exitForStatement(WenyanParser::ForStatementContext *context) override {
        WenyanBaseListener::exitForStatement(context);
        AllocaInst * oldVal = namedValues["index"];
        namedValues["index"] = context->alloca;
        Value *stepV = ConstantFP::get(theLLVMContext,APFloat(1.0));
        Value *endV = context->number()->value;
        Value *curVar = builder.CreateLoad(context->alloca,"index");
        Value *nextVar = builder.CreateFAdd(curVar,stepV,"nextVar");
        builder.CreateStore(nextVar,context->alloca);
        Value *l = builder.CreateFCmpULT(nextVar,context->number()->value,"cmptmp");
        endV = builder.CreateUIToFP(l,Type::getDoubleTy(theLLVMContext),"booltmp");
        endV = builder.CreateFCmpONE(endV,ConstantFP::get(theLLVMContext,APFloat(0.0)),"loopcond");
        BasicBlock *loopEndBB = builder.GetInsertBlock();
        BasicBlock *afterBB = BasicBlock::Create(theLLVMContext,"afterloop",context->theFunction);
        builder.CreateCondBr(endV,context->loopBB,afterBB);
        builder.SetInsertPoint(afterBB);
        if(oldVal)
            namedValues["index"] = oldVal;
        else
            namedValues.erase("index");
    }

    void exitReturnStatement(WenyanParser::ReturnStatementContext *context) override {
        WenyanBaseListener::exitReturnStatement(context);
        builder.CreateRet(context->expression()->value);
    }

    void enterVariable(WenyanParser::VariableContext *context) override {
        WenyanBaseListener::enterVariable(context);
        std::string name;
        chineseConvertPy(context->getText(), name);
        Value *v = namedValues[name];
        if(v){
            context->value = builder.CreateLoad(v,name.c_str());
            return;
        }
        auto fi = globalValues.find(name);
        if(fi!=globalValues.end())
            context->value = fi->second;
    }


    void exitApplyFunction(WenyanParser::ApplyFunctionContext *context) override {
        WenyanBaseListener::enterApplyFunction(context);
        std::vector<Value *> argsV;
        for(int i =0;i<context->funcVars().size();++i){
            Value *v = context->funcVars()[i]->sn?context->funcVars()[i]->sn->value:context->funcVars()[i]->sv->value;
            argsV.push_back(v);
        }
        std::string callee;
        chineseConvertPy(context->fv->getText(),callee);
        Function *calleeF = getFunction(callee);
        Value *v = builder.CreateCall(calleeF,argsV,"calltmp");
        context->value = v;
    }

    void enterDeclarefunction(WenyanParser::DeclarefunctionContext *context) override {
        WenyanBaseListener::enterDeclarefunction(context);
        // gen Proto
        getFunction(std::to_string(anonFnNum-1))->eraseFromParent();
        functionProtos.erase(std::to_string(anonFnNum-1));
        std::vector<std::string> argNames;
        if(context->variables()){
            std::vector<WenyanParser::VariableContext *> vars=context->variables()->variable();
            for(int i=0;i<vars.size();++i){
                std::string argName;
                chineseConvertPy(vars[i]->getText(), argName);
                argNames.push_back(argName);
            }
        }
        std::string fnName;
        chineseConvertPy(context->variable()->getText(), fnName);
        std::unique_ptr<PrototypeAST> proto = llvm::make_unique<PrototypeAST>(fnName,std::move(argNames));
        functionProtos[fnName] = std::move(proto);
        Function *theFunction = getFunction(fnName);
        if(!theFunction)
            return;
        BasicBlock *bb = BasicBlock::Create(theLLVMContext,"entry",theFunction);
        builder.SetInsertPoint(bb);
        namedValues.clear();
        for(auto &arg:theFunction->args()){
            AllocaInst *alloca = CreateEntryBlockAlloca(theFunction,arg.getName());
            builder.CreateStore(&arg,alloca);
            namedValues[arg.getName()] = alloca;
        }
        context->theFunction = theFunction;
        curCtx = FUNCTION;
    }

    void exitDeclarefunction(WenyanParser::DeclarefunctionContext *context) override {
        WenyanBaseListener::exitDeclarefunction(context);
        if(context->block()->statement().back()->returnStatement()){
            verifyFunction(*context->theFunction);
            fpm->run(*context->theFunction);
            curCtx = GLOBAL;
            jit->addModule(std::move(module));
            initializeModuleAndpassManager();
            return;
        }
        if(Value *retVal = context->block()->value){
            builder.CreateRet(retVal);
            verifyFunction(*context->theFunction);
            fpm->run(*context->theFunction);
            curCtx = GLOBAL;
            jit->addModule(std::move(module));
            initializeModuleAndpassManager();
            return;
        }
        curCtx = GLOBAL;
        context->theFunction->eraseFromParent();
    }

    void exitDeclareNumber(WenyanParser::DeclareNumberContext *context) override {
        WenyanBaseListener::enterDeclareNumber(context);
        if(!getFunction(std::to_string(anonFnNum-1))){
            Function *theFunction = builder.GetInsertBlock()->getParent();
            for (unsigned i =0,e= context->variable().size();i!=e;++i){
                std::string varName;
                chineseConvertPy(context->variable()[i]->getText(),varName);
                Value *initVal = context->expression()[i]->value;
                if(!initVal){
                    initVal = ConstantFP::get(theLLVMContext,APFloat(0.0));
                }
                AllocaInst *alloca = CreateEntryBlockAlloca(theFunction,varName);
                builder.CreateStore(initVal,alloca);
                namedValues[varName] = alloca;
            }
        } else {
            Function *theFunction = builder.GetInsertBlock()->getParent();
            for (unsigned i =0,e= context->variable().size();i!=e;++i){
                std::string varName;
                chineseConvertPy(context->variable()[i]->getText(),varName);
                Value *initVal = context->expression()[i]->value;
                if(!initVal){
                    initVal = ConstantFP::get(theLLVMContext,APFloat(0.0));
                }
                globalValues[varName] = initVal;
            }
        }
        context->value = context->expression()[0]->value;
    }

    void enterNumber(WenyanParser::NumberContext *context) override {
        WenyanBaseListener::enterNumber(context);
        context->value = ConstantFP::get(Type::getDoubleTy(theLLVMContext),chineseNum2num(context->getText()));
    }

    void exitAssignStatement(WenyanParser::AssignStatementContext *context) override {
        WenyanBaseListener::enterAssignStatement(context);
        std::string varName;
        chineseConvertPy(context->variable()->getText(),varName);
        Value *variable = namedValues[varName];
        if(!variable){
            if(globalValues[varName]){
                globalValues[varName] = context->expression()->value;
                context->value = context->expression()->value;
                return;
            } else {
                return;
            }
        }
        if(!context->expression()->value){
            fprintf(stderr,"expression value is null");
        }
        builder.CreateStore(context->expression()->value,variable);
        context->value = context->expression()->value;
    }
};
*/


using namespace antlr4;
using namespace antlr4::tree;
using namespace llvm;


static llvm::cl::opt<std::string> Input(llvm::cl::Positional,
                                        llvm::cl::Required,
                                        llvm::cl::desc("<Input file location>"));

static llvm::cl::opt<std::string> Output(llvm::cl::Positional,
                                         llvm::cl::Required,
                                         llvm::cl::desc("<Output file location>"));

namespace CodeGen {
    std::unique_ptr context = std::make_unique<llvm::LLVMContext>();
    std::unique_ptr module = std::make_unique<llvm::Module>("Compiler 426", *context);
    std::unique_ptr<IRBuilder<>> builder = std::make_unique<llvm::IRBuilder<>>(*context);
    Type *voidTy = Type::getVoidTy(module->getContext());
    Type *int32Ty = Type::getInt32Ty(module->getContext());
    Constant *int32Zero = ConstantInt::get(int32Ty, 21, true);
    StringMap<Type *> typeMap{{"number", int32Ty}};
    StringMap<Value *> namedValues;

    class ToIRVisitor : public grammar426BaseVisitor {
    public:
        class FunctionParameterVisitor : public grammar426BaseVisitor {
            StringMap<Type *> typeMap;
        public:
            explicit FunctionParameterVisitor(StringMap<Type *> typeMap) : typeMap(typeMap) {}

            antlrcpp::Any visitFunctionDeclaration(grammar426Parser::FunctionDeclarationContext *ctx) override {
                if (ctx->formalParameterList()) {
                    return visitFormalParameterList(ctx->formalParameterList());
                } else {
                    llvm::SmallVector<Type *> parametersTypes;
                    llvm::SmallVector<std::string> parametersNames;
                    return std::pair(parametersTypes, parametersNames);
                }
            }

            antlrcpp::Any visitFormalParameterList(grammar426Parser::FormalParameterListContext *ctx) override {
                llvm::SmallVector<Type *> parametersTypes;
                llvm::SmallVector<std::string> parametersNames;

                auto &&children = ctx->children;
                size_t sz = children.size();
                parametersNames.reserve(sz / 2);
                parametersTypes.reserve(sz / 2);
                for (int i = 0; i < sz; ++i) {
                    auto &&typeName = children[i]->getText();
                    DEBUG(std::cout << "parametertypeName" << typeName);
                    Type *type = typeMap[typeName];
                    assert(type && "Should declare parameter type before use");
                    parametersTypes.emplace_back(type);
                    ++i;
                    std::string name = children[i]->getText();
                    DEBUG(std::cout << "parameterName" << name);
                    parametersNames.emplace_back(std::move(name));
                }
                return std::pair(parametersTypes, parametersNames);
            }
        };

        class FunctionBodyVisitor : public grammar426BaseVisitor {
        public:
            antlrcpp::Any visitVariableStatement(grammar426Parser::VariableStatementContext *ctx) override {
                Type *variableType = typeMap[ctx->Type()->getText()];
                assert(variableType && "Should declare type of variable before use");
                for (auto &&variableDecl: ctx->variableDeclarationList()->variableDeclaration()) {
                    // avoid redefinition: Search the variable in scope table.
                    auto &&variableName = variableDecl->Identifier()->getText();
                    assert(namedValues.find(variableName) == namedValues.end() && "redefinition");

                    if (variableDecl->initialiser()) {
                        auto variableInitialValueText = std::stoi(
                                variableDecl->initialiser()->singleExpression()->getText());
                        DEBUG(std::cout << "variableInitialValueText: " << variableInitialValueText);
                        namedValues[variableName] = ConstantInt::get(int32Ty, variableInitialValueText, true);
                    }
                }
                return std::any();
            }

            antlrcpp::Any visitReturnStatement(grammar426Parser::ReturnStatementContext *ctx) override {
                //TEST: variable Name
                auto &&vname = ctx->expressionSequence()->getText();
                DEBUG(std::cout << "return value: "<<vname);
                Value *v = namedValues[vname];
                return v;
            }
        };

        antlrcpp::Any visitFunctionDeclaration(grammar426Parser::FunctionDeclarationContext *ctx) override {
            Type *functionReturnType = typeMap[ctx->Type()->getText()];
            assert(functionReturnType && "Should declare return type of functionType before use");
            auto visitor = FunctionParameterVisitor(typeMap);

            // get parameters by visiting formalParameterList
            auto parameters = std::any_cast<std::pair<llvm::SmallVector<Type *>, llvm::SmallVector<std::string>>>(
                    ctx->accept(
                            &visitor));

            auto &&parametersTypes = parameters.first;
            auto &&parametersNames = parameters.second;
            FunctionType *functionType = FunctionType::get(functionReturnType, parameters.first, false);
            auto &&functionName = ctx->Identifier()->getText();
            DEBUG(std::cout << "Function name: " << functionName);
            Function *func = Function::Create(functionType, GlobalValue::ExternalLinkage, functionName, *module);
            for (int i = 0; auto &&arg: func->args()) {
                arg.setName(parametersNames[i]);
            }

            BasicBlock *basicBlock = BasicBlock::Create(module->getContext(), "entry", func);
            builder->SetInsertPoint(basicBlock);

            FunctionBodyVisitor bodyVisitor;
            if (Value *ret = std::any_cast<llvm::Value *>(ctx->functionBody()->accept(&bodyVisitor))) {
                builder->CreateRet(ret);
            }
            return func;
        }

    };

    void compile(grammar426Parser::ProgramContext *tree) {
        ToIRVisitor toir;

        tree->accept(&toir);

        std::error_code code;
        raw_fd_ostream os(Output.getValue(), code);
        module->print(os, nullptr);
    }
};
int main(int argc, const char **argv) {
    llvm::InitLLVM X(argc, argv);
    llvm::cl::ParseCommandLineOptions(argc, argv, "compiler426 - the compiler I want to make.");

    ANTLRFileStream fileStream;
    auto filename = Input.getValue();
    fileStream.loadFromFile(filename);

    grammar426Lexer lexer(&fileStream);
    CommonTokenStream tokens(&lexer);

    grammar426Parser parser(&tokens);
    grammar426Parser::ProgramContext *tree = parser.program();

    CodeGen::compile(tree);
    return 0;
}