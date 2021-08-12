import pandas as pd
import numpy as np
from sklearn.base import BaseEstimator
from sklearn.metrics import accuracy_score
from collections import Counter 
# Если не установлен - надо установить (версия 2.2.2: pip install category_encoders==2.2.2)
from category_encoders import TargetEncoder

def compute_criterion(target_vector: np.array, feature_vector: int, threshold: float, criterion: str='gini') -> float:
    
    """
    Args:
        target_vector: вектор таргетов (бинарный)
        feature_vector: вектор с конкретной фичёй объектов (вещественный)
        threshold: погор для разбиение на левое и правое поддеревья
        criterion: какой критерий считать ("gini" или "entropy") 
    Returns:
        Q: критерий расщепления (максимизируя который мы выбираем оптимальное разбиение листа)
    """
    
    assert criterion in ['gini', 'entropy'], "Критерий может быть только 'gini' или 'entropy'!"
    
    # Ваш код здесь:
    left_target_vector = target_vector[feature_vector[:] < threshold]
    right_target_vector = target_vector[feature_vector[:] >= threshold]
    N_left = len(left_target_vector)
    N_right = len(right_target_vector)
    N_all = N_left + N_right
    probs_root_dict = Counter(target_vector) 
    probs_root_dict = dict([(key, val/len(target_vector)) for key, val in probs_root_dict.items()])
    
    probs_left_dict = Counter(left_target_vector) 
    probs_left_dict = dict([(key, val/len(left_target_vector)) for key, val in probs_left_dict.items()])
    
    probs_right_dict = Counter(right_target_vector) 
    probs_right_dict = dict([(key, val/len(right_target_vector)) for key, val in probs_right_dict.items()])
    
    if criterion == 'gini':
        H_root = np.sum([p * (1 - p) for p in probs_root_dict.values()])
        H_left = np.sum([p * (1 - p) for p in probs_left_dict.values()])
        H_right = np.sum([p * (1 - p) for p in probs_right_dict.values()])
        
    if criterion == 'entropy':
        H_root = (-1)*np.sum([p*np.log2(p) for p in probs_root_dict.values()])
        H_left = (-1)*np.sum([p*np.log2(p)  for p in probs_left_dict.values()])
        H_right = (-1)*np.sum([p*np.log2(p)  for p in probs_right_dict.values()])
    
    Q = H_root - (N_left / N_all) * H_left - (N_right / N_all) * H_right
    
    return Q



def find_best_split(feature_vector: np.array, target_vector: np.array, criterion: str='gini'):
    
    """
    Указания:
    * Пороги, приводящие к попаданию в одно из поддеревьев пустого множества объектов, не рассматриваются.
    * В качестве порогов, нужно брать среднее двух сосдених (при сортировке) значений признака
    * В случае константного признака функция должны выдавать: None, None, None, 0
    * При одинаковых приростах Джини нужно выбирать минимальный сплит.

    Args:
        criterion: может быть "gini" или "entropy"
        feature_vector: вещественнозначный вектор значений признака
        target_vector: вектор классов объектов (бинарный),  len(feature_vector) == len(target_vector)
    Returns:
        thresholds: (np.array) отсортированный по возрастанию вектор со всеми возможными порогами, по которым объекты можно
                     разделить на две различные подвыборки, или поддерева
        criterion_vals: (np.array) вектор со значениями критерия Джини/энтропийного критерия для каждого из порогов 
                в thresholds. len(criterion_vals) == len(thresholds)
        threshold_best: (float) оптимальный порог
        criterion_best: (float) оптимальное значение критерия 
    """
    
    assert criterion in ['gini', 'entropy'], "может быть 'gini' или 'entropy'!"
    
    # Ваш код здесь:
    if len(np.unique(feature_vector)) != 1:
        criterion_best, threshold_best = None, None
        unq_vals = np.sort(np.unique(feature_vector[:]))
        thresholds = np.array([(i + j)/2 for i, j in zip(unq_vals, unq_vals[1:])])
        criterion_vals = []
        for threshold in thresholds:
            current_gain = compute_criterion(feature_vector=feature_vector, target_vector=target_vector, threshold=threshold, criterion = criterion)
            criterion_vals.append(current_gain)
            if criterion_best is None:
                criterion_best = current_gain
                threshold_best = threshold
            if current_gain > criterion_best:
                criterion_best = current_gain
                threshold_best = threshold
        return thresholds, criterion_vals, threshold_best, criterion_best
    else:
        return None, None, None, 0


class DecisionTree:
    def __init__(self, feature_types: list, criterion: str='gini', 
                 max_depth: int=None, min_samples_split: int=None, min_samples_leaf: int=None):
        
        """
        Args:
            feature_types: список типов фичей (может состоять из 'real' и "categorical")
            criterion: может быть 'gini' или "entropy"
            max_depth: максимальная глубина дерева
            min_samples_split: минимальное число объектов в листе, чтобы можно было расщиплять этот лист
            min_samples_leaf: минимальное число объектов в полученных листьях
        """
        
        self._feature_types = feature_types
        self._tree = {}
        self.target_encodings = {}
        self._criterion = criterion
        self.max_depth = max_depth
        self._min_samples_split = min_samples_split
        self._min_samples_leaf = min_samples_leaf

    def _fit_node(self, sub_X: np.array, sub_y: np.array, node: dict):
        # Если все объекты одного класса, то текущий node это лист
        if np.all(sub_y == sub_y[0]):
            node['type'] = 'terminal'
            node['class'] = sub_y[0]
            return

        best_criterion, best_feature_number, best_threshold = None, None, None
        
        for feature_number in range(0, sub_X.shape[1]):
            if self._feature_types[feature_number] == 'real':
                feature_vector = sub_X[:, feature_number]
            elif self._feature_types[feature_number] == 'categorical':
                feature_vector = self.target_encodings[feature_number].transform(sub_X[:, feature_number]).values.ravel()
            else:
                raise ValueError('feature_type может быть "real" или "categorical"')
            
            _, _, curr_threshold, curr_crit = find_best_split(
                        feature_vector=feature_vector,
                        target_vector=sub_y,
                        criterion=self._criterion
                    )
            if best_criterion is None or curr_crit > best_criterion:
                best_criterion = curr_crit
                best_threshold = curr_threshold
                best_feature_number = feature_number
                
                split = feature_vector < best_threshold if best_threshold is not None else None

        # Если в ноде у всех фичей константное значение - пришли в лист (так как не можем расщипить)
        if best_threshold is None:
            node['type'] = 'terminal'
            node['class'] = Counter(sub_y).most_common(1)[0][0]
            return

        node['type'] = 'nonterminal'
        node['feature_type'] = self._feature_types[best_feature_number]
        node['feature_number'] = best_feature_number
        node['threshold'] = best_threshold
                
        node['left_child'], node['right_child'] = {}, {}
        # Рекурсивно обучаем левое и правое поддеревья
        self._fit_node(sub_X=sub_X[split], sub_y=sub_y[split], node=node['left_child'])
        self._fit_node(sub_X=sub_X[np.logical_not(split)], sub_y=sub_y[np.logical_not(split)], 
                       node=node['right_child'])

    def _predict_node(self, x: np.array, node: dict):
        
        """
        Должен либо вернуть класс для объекта х, либо рекурсивно просеить его в левое или правое поддерево.
        """
        result_class_for_iteration_i = None
        
        # попробуем учесть априорную информацию глубине дерева как гипер параметре моделии,
        # иначе поставим очень большое число итераций, чтобы наверняка все таргеты были распределены по классам
        # (в случае, если итераций мало, например 2 (глубина дерева равна 2), получаем, что много неопределённых
        # значений (None))
        
        if self.max_depth == None:
            iterations = 10000
        else:
            iterations = self.max_depth
            
        for i in range(iterations):
            if node['type'] == 'terminal':
                result_class_for_iteration_i = node['class']
                break
            else:
                feature_split_for_iteration_i =  node['feature_number']
                if node['feature_type'] == 'real':
                    real_split_for_iteration_i = node['threshold']
                    if x[feature_split_for_iteration_i] < real_split_for_iteration_i:
                        node = node['left_child']
                    else:
                        node = node['right_child']
                else:
                    x_encoded = self.target_encodings[feature_split_for_iteration_i].transform([x[feature_split_for_iteration_i]]).values
                    
                    if x_encoded < node['threshold']:
                        node = node['left_child']
                    else:
                        node = node['right_child']
        return result_class_for_iteration_i
        

    def fit(self, X: np.array, y: np.array):

	# Сначала создаём словарь для таргет энкодингов
        for feature_number, feature_type in enumerate(self._feature_types):
            if feature_type == 'categorical':
                target_enc = TargetEncoder(cols=0).fit(X[:, feature_number], y)
                self.target_encodings[feature_number] = target_enc

        self._fit_node(sub_X=X, sub_y=y, node=self._tree)

    def predict(self, X: np.array):
        assert self._tree != {}, "Cначала обучите модель!"
        predicted = []
        for x in X:
            predicted.append(self._predict_node(x=x, node=self._tree))
        return np.array(predicted)
    
    def cv_result(self, X: np.array, y: np.array, n_folds: int=10, scorer: callable=accuracy_score):
        
        """
        Вспомогательный метод для задания в практической. Этот метод получает оценку scorer функции для модели по n_folds фолдам.
        """
        
        # Создаём индексы объектов по фолдам
        idxs_by_folds = np.array_split(
            np.random.permutation(len(X)), n_folds
        )
        
        train_val_idxs_dict = {
            fold_number: {
                'train_idxs': np.array(list(set(np.arange(len(X))) - set(idxs_by_folds[fold_number]))),
                'val_idxs': idxs_by_folds[fold_number]
            } for fold_number in range(n_folds)
        }
        
        cv_results = []
        for fold_number in range(n_folds):
            
            self._tree = {}
            self.fit(X=X[train_val_idxs_dict[fold_number]['train_idxs']], 
                             y=y[train_val_idxs_dict[fold_number]['train_idxs']])
            y_pred = self.predict(X=X[train_val_idxs_dict[fold_number]['val_idxs']])
            y_true = y[train_val_idxs_dict[fold_number]['val_idxs']]
            
            cv_results.append(
                scorer(y_true=y_true, y_pred=y_pred)
            )
        return cv_results
