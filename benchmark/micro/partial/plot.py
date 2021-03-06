import numpy as np
import matplotlib as mpl

latex_output = False
show_regression = True

tb_color = '#f4a582'
cr_color = '#0571b0'


if latex_output:
    mpl.use('PDF')
    mpl.rc('font',**{'family':'serif','serif':['Times'], 'size' : 15})
    mpl.rc('text', usetex=True)
else:
    mpl.rc('font',**{'family':'serif','serif':['Times'], 'size' : 15})
    #mpl.rcParams.update({'font.size': 15})
    
import matplotlib.pyplot  as plt
import matplotlib.patches as mpatch
import matplotlib.lines   as mlines

def sort_for_violin_plot(data_set, key, value):
    data = {c : [] for c in data_set[key]}
    for datum in data_set:
        data[datum[key]].append(datum[value])
    data = data.items()
    data.sort(key=(lambda x: x[0]))
    return tuple([list(t) for t in zip(*data)])

def set_violin_color_label(vplot, color, label):
    for p in vplot['bodies']:
        p.set_color(color)
        p.set_alpha(.6)
    return mpatch.Patch(color=color, label=label)

def violin_plot(data, x, y, widths, color, label, do_regression=None):
    pos, times = sort_for_violin_plot(data, x, y)
    vplt = plt.violinplot(times
                          , positions=pos
                          , widths=widths
                          , showextrema=False
                          , showmedians=False
                          , showmeans=False)
    
    vplt_fake = set_violin_color_label(vplt, color, label)
    
    if do_regression and show_regression:
        color, x_name, l_label, eq_x, eq_y = do_regression
        lreg = np.polyfit(data[x] , data[y] , 1)
        lplt = plt.plot(data[x]
                        , np.polyval(lreg, data[x])
                        , linestyle="-"
                        , color=color)
        
        if lreg[1] < 0 :
            lplt_eq = "{:.2f}{} - {:.2f}".format(lreg[0], x_name, (-1.0 * lreg[1]))
        else:
            lplt_eq = "{:.2f}{} + {:.2f}".format(lreg[0], x_name, lreg[1])
            
        lplt_fake = mlines.Line2D([],[],color=color, label=l_label)
        lplt_txt  = plt.text(eq_x, eq_y, lplt_eq, color=color)
        return (vplt_fake, lplt_fake)
    else:
        return (vplt_fake, None)

# Function cast analysis
# Reference cast analysis
def cast_plot(fignum, test, test_action, rx, ry, calc_plot_bounds=None):
    plt.figure(fignum)
    
    type_based_cast_data = np.genfromtxt('out/{}_Type-Based.txt'.format(test)
                                         , dtype=(int, int, float)
                                         , names="types, coercions, time"
                                         , usecols=("coercions, time"))

    reg_params = ("purple" , "$N_c$" , "Linear Model of Type-Based"
                  , rx ,  ry - 100)
    type_based_vplt, type_based_lplt = violin_plot(type_based_cast_data
                                                   , "coercions"
                                                   , "time"
                                                   , 7
                                                   , tb_color
                                                   , "Type-Based Casts"
                                                   ,do_regression=reg_params)

    coercions_cast_data = np.genfromtxt('out/{}_Coercions.txt'.format(test)
                                        , dtype=(int, int, float)
                                        , names=("types", "coercions", "time")
                                        , usecols=("coercions", "time"))

    reg_params = ("red" , "$N_c$" , "Linear Model of Coercions" , rx ,  ry)
    coercions_vplt, coercions_lplt = violin_plot(coercions_cast_data
                                                 ,"coercions"
                                                 ,"time"
                                                 , 7
                                                 , cr_color
                                                 , "Coercions"
                                                 ,do_regression=reg_params)
    
    if calc_plot_bounds:
        plt.axis(calc_plot_bounds(min(coercions_cast_data["coercions"])
                                  , max(coercions_cast_data["coercions"])
                                  , min(coercions_cast_data["time"])
                                  , max(coercions_cast_data["time"])))
        
    plt.xlabel('Number of nodes in Coercion resulting from types')
    plt.ylabel('Time in $ns$ to perform {}'.format(test_action))

    hs = [ type_based_vplt
           ,coercions_vplt]
    
    if show_regression:
        hs.extend([coercions_lplt, type_based_lplt])

    plt.legend(handles=hs
               , loc="upper left"
               , shadow=True)

    if latex_output:
        plt.savefig('out/{}-results'.format(test), bbox_inches='tight')
        
    plt.savefig('out/{}-results.png'.format(test), bbox_inches='tight')

    print test, "type-based", np.mean(type_based_cast_data["time"])
    print test, "coercions", np.mean(coercions_cast_data["time"])

cast_plot(1, "fn-cast", "function cast", 80, 450,
          lambda lx, hx, ly, hy: [lx - 10, hx + 10, -25, hy + 10])

cast_plot(2, "ref-cast", "reference cast", 40, 200, 
          lambda lx, hx, ly, hy: [lx - 10, hx + 10, 0, hy + 10])
    
# type_based_cast_data = np.genfromtxt('out/fn-cast_Type-Based.txt'
#                                      , dtype=(int, int, float)
#                                      , names="types, coercions, time"
#                                      , usecols=("coercions, time"))                     

# type_based_vplt = violin_plot(type_based_cast_data
#                               , "coercions"
#                               , "time"
#                               , 7
#                               , tb_color
#                               , "Type-Based Casts")


# coercions_cast_data = np.genfromtxt('out/fn-cast_Coercions.txt',
#                                     dtype=(int, int, float),
#                                     names=("types", "coercions", "time"),
#                                     usecols=("coercions", "time"))

# coercions_vplt, coercions_lplt = violin_plot(coercions_cast_data
#                                              ,"coercions"
#                                              ,"time"
#                                              , 7
#                                              , "green"
#                                              , "Coercions"
#                                              ,do_regression=("red"
#                                                              , "$N_c$"
#                                                              , "Linear Model of Coercions"
#                                                              ,  80
#                                                              ,  325))

# plt.axis([0, max(coercions_cast_data["coercions"]) + 10
#           , -100, max(coercions_cast_data["time"]) + 100])
# plt.xlabel('Number of nodes in Coercion resulting from types ($N_c$)')
# plt.ylabel('Time in $ns$ per cast')
# legend = plt.legend(handles=[ type_based_vplt
#                               ,coercions_vplt
#                               ,coercions_lplt]
#                     , loc="upper left"
#                     , shadow=True)

# if latex_output:
#     plt.savefig('out/fn-cast-results', bbox_inches='tight')

# plt.savefig('out/fn-cast-results.png', bbox_inches='tight')

# Function application analysis
# Reference Write Read analysis
def use_by_casts_plot(fignum, test, rx, ry, test_action, calc_bounds=None):
    plt.figure(fignum)
    t_data = np.genfromtxt('out/{}-Type-Based-coercions=15.txt'.format(test)
                           , dtype=(int, int, int, float, float)
                           , names="casts, tsize, csize, trun, titer"
                           , usecols=("casts, titer"))
    c_data = np.genfromtxt('out/{}-Coercions-coercions=15.txt'.format(test)
                           , dtype=(int, int, int, float, float)
                           , names="casts, tsize, csize, trun, titer"
                           , usecols=("casts, titer"))

    reg_params = ("red", "$N_i$", "Linear Model of Type-Based Casts", rx, ry)
    
    t_vplt, t_lplt = violin_plot(t_data
                         ,"casts"
                         ,"titer"
                         , 0.5
                         , tb_color
                         , "Type-Based Casts"
                         ,do_regression=reg_params)

    reg_params = ("purple", "$N_i$", "Linear Model of Coercions", rx, ry - 100)
    c_vplt, c_lplt = violin_plot(c_data ,"casts" ,"titer" , 0.5
                               , cr_color , "Coercions", do_regression=reg_params)

    if calc_bounds:
        plt.axis(calc_bounds(min(c_data["casts"]),
                             max(c_data["casts"]),
                             min(t_data["titer"]),
                             max(t_data["titer"])))
    plt.xlabel('Number of casts before the {}'.format(test_action))
    plt.ylabel('Time in $ns$ to {}'.format(test_action))

    hs = [ t_vplt ,c_vplt]

    if show_regression:
        hs.extend([t_lplt, c_lplt])
    
    plt.legend(handles=hs
               , loc="upper left"
               , shadow=True)

    if latex_output:
        plt.savefig('out/{}-by-casts-coercions=15'.format(test), bbox_inches='tight')

    plt.savefig('out/{}-by-casts-coercions=15.png'.format(test), bbox_inches='tight')
    c_data_odd = [t for c, t in c_data if not 0 == c % 2]
    c_data_even = [t for c, t in c_data if 0 == c % 2]
    
    print test, "type-based", np.mean(t_data["titer"])
    print test, "coercions odd", np.mean(c_data_odd)
    print test, "coercions even", np.mean(c_data_even)
    
use_by_casts_plot(3, "fn-app", 10, 275, "apply function",
                  lambda lx, hx, ly, hy: [-.5, hx + .5, -10, 1250])

use_by_casts_plot(4, "ref-write-read", 1, 300, "write and read reference",
                  lambda lx, hx, ly, hy: [-.5, hx + .5, -10, 1250])               

               
def use_by_crcns_plot(fignum, test, rx, ry, test_action, w, calc_bounds):
    plt.figure(fignum)
    
    t_data = np.genfromtxt('out/{}-Type-Based-casts=1.txt'.format(test)
                           , dtype=(int, int, int, float, float)
                           , names="casts, types, coercions, trun, titer"
                           , usecols=("coercions, titer"))
    c_data = np.genfromtxt('out/{}-Coercions-casts=1.txt'.format(test)
                           , dtype=(int, int, int, float, float)
                           , names="casts, types, coercions, trun, titer"
                           , usecols=("coercions, titer"))

    reg_params = ("purple" , "$N_c$" , "Linear Model of Type-Based" , rx ,  ry - 100)
    t_vplt, t_lplt = violin_plot(t_data ,"coercions" ,"titer"
                               ,w ,tb_color ,"Type-Based Casts"
                               , do_regression=reg_params)

    reg_params = ("red" , "$N_c$" , "Linear Model of Coercions" , rx ,  ry)
    c_vplt, c_lplt = violin_plot(c_data ,"coercions" ,"titer" ,w
                                 ,cr_color ,"Coercions", do_regression=reg_params)

    plt.axis(calc_bounds(min(c_data["coercions"]),
                         max(c_data["coercions"]),
                         min(c_data["titer"]),
                         max(c_data["titer"])))
    plt.xlabel('Nodes in the Coercions needed to represent cast')
    plt.ylabel('Time in $ns$ to {}'.format(test_action))


    hs = [t_vplt, c_vplt]

    if show_regression :
        hs.extend([t_lplt, c_lplt])
    
    legend = plt.legend(handles=hs , loc="upper left" , shadow=True)

    if latex_output:
        plt.savefig('out/{}-by-coercions-casts=1'.format(test), bbox_inches='tight')
        
    plt.savefig('out/{}-by-coercions-casts=1.png'.format(test), bbox_inches='tight')

    t_data_gt10 = [t for (c, t) in t_data if c > 10]
    print test, "type-based", np.mean(t_data["titer"])
    print test, "type-based > 10", np.mean(t_data_gt10)
    print test, "coercions", np.mean(c_data["titer"])
    
    
use_by_crcns_plot(5, "fn-app", 40, 140, "apply function", 5,
                  lambda lx, hx, ly, hy: [lx-10, hx+10, ly-10, hy+10])
use_by_crcns_plot(6, "ref-write-read", 40, 350, "write and read reference", 5,
                  lambda lx, hx, ly, hy: [lx-10, hx+10, ly-10, hy +10])


plt.show()
